defmodule Harald.Spec.Helpers do
  @moduledoc false

  use ExUnitProperties
  alias Harald.Spec

  @doc false
  def define_generators(processed_spec) do
    Enum.reduce(Spec.get(), [], fn
      {:packets, _}, acc ->
        acc

      {key, value}, acc when key in [:commands, :events] ->
        Enum.reduce(value, acc, fn
          %{
            bin_pieces: bin_pieces,
            gen_body: gen_body,
            gen_clauses: gen_clauses,
            keys: keys,
            parameters: parameters,
            name: name,
            size: size
          },
          acc ->
            ast =
              quote location: :keep do
                def generate(unquote(name)) do
                  gen all(unquote_splicing(gen_clauses)) do
                    unquote(gen_body)
                  end
                end
              end

            acc ++ [ast]
        end)
    end)
  end

  @doc false
  def define_serializers(processed_spec) do
    Enum.reduce(Spec.get(), [], fn
      {:packets, _}, acc ->
        acc

      {key, value}, acc when key in [:commands, :events] ->
        Enum.reduce(value, acc, fn
          %{
            bin_pieces: bin_pieces,
            gen_body: gen_body,
            gen_clauses: gen_clauses,
            keys: keys,
            parameters: parameters,
            name: name,
            size: size
          },
          acc ->
            ast =
              quote location: :keep do
                def serialize(unquote(parameters)) do
                  {:ok, unquote(bin_pieces)}
                end

                def deserialize(unquote(bin_pieces)) do
                  {:ok, unquote(parameters)}
                end
              end

            acc ++ [ast]
        end)
    end)
  end

  @doc false
  def process_spec(bt_spec) do
    Enum.reduce(bt_spec, %{commands: nil, events: nil}, fn
      {:packets, _}, acc ->
        acc

      {:command_groups, command_groups}, acc ->
        %{acc | commands: process_command_groups(command_groups)}

      {:events, events}, acc ->
        %{acc | events: process_events(events)}
    end)
  end

  defp process_command_groups(commands) do
    Enum.reduce(commands, [], fn {ogf, commands}, acc ->
      Enum.reduce(commands, acc, fn command, acc ->
        processed_parameters = process_parameters(command.parameters)

        gen_clause = {:<-, [], [{:ocf, [], Elixir}], quote(do: StreamData.constant(command.ocf))}
        command_name = command.command
        keys = [{:command, command_name} | processed_parameters.keys]
        <<opcode::size(16)>> = <<ogf::size(6), command.ocf::size(10)>>
        opcode = <<opcode::little-size(16)>>

        gen_body =
          {:<<>>, [], [1, opcode, processed_parameters.size | processed_parameters.gen_body]}

        bin_pieces = [1, opcode, processed_parameters.size | processed_parameters.bin_pieces]

        [
          %{
            processed_parameters
            | bin_pieces: {:<<>>, [], processed_parameters.bin_pieces},
              keys: keys,
              name: command_name,
              parameters: {:%{}, [], keys},
              gen_body: gen_body,
              gen_clauses: [gen_clause | processed_parameters.gen_clauses]
          }
          | acc
        ]
      end)
    end)

    []
  end

  defp process_events(events) do
    Enum.reduce(events, {[], []}, fn
      %{subevents: subevents}, acc ->
        Enum.reduce(subevents, acc, fn event, acc -> [process_event(event) | acc] end)

      event, acc ->
        [process_event(event) | acc]
    end)
  end

  defp process_event(event) do
    processed_parameters = process_parameters(event.parameters)
    event_code = event_code(event)

    gen_clause =
      {:<-, [], [{:event_code, [], Elixir}], quote(do: StreamData.constant(event_code))}

    event_name = event.event
    keys = [{:event, event_name} | processed_parameters.keys]

    gen_body =
      {:<<>>, [], [4, event_code, processed_parameters.size | processed_parameters.gen_body]}

    bin_pieces = [4, event_code, processed_parameters.size | processed_parameters.bin_pieces]

    %{
      processed_parameters
      | bin_pieces: {:<<>>, [], processed_parameters.bin_pieces},
        keys: keys,
        name: event_name,
        parameters: {:%{}, [], keys},
        gen_body: gen_body,
        gen_clauses: [gen_clause | processed_parameters.gen_clauses]
    }
  end

  defp event_code(%{event_code: event_code}), do: event_code

  defp event_code(%{parameters: [%{name: :subevent_code, value: subevent_code} | _]}) do
    subevent_code
  end

  defp process_parameters(parameters, acc \\ {[], [], 0, [], []}) do
    {keys, bin_pieces, size, gen_clauses, gen_body} =
      Enum.reduce(parameters, acc, fn parameter,
                                      {acc_keys, acc_bin_pieces, acc_size, acc_gen_clauses,
                                       acc_gen_body} ->
        size = add(parameter.size, acc_size)

        case parameter.type do
          :arrayed_data ->
            {keys, bin_pieces, size, gen_clauses, gen_body} =
              process_parameters(parameter.parameters)

            {
              keys ++ acc_keys,
              bin_pieces ++ acc_bin_pieces,
              size,
              gen_clauses ++ acc_gen_clauses,
              gen_body ++ acc_gen_body
            }

          _ ->
            processed_parameter = process_parameter(parameter)

            {
              [processed_parameter.key | acc_keys],
              [processed_parameter.bin_piece | acc_bin_pieces],
              processed_parameter.size,
              [processed_parameter.gen_clause | acc_gen_clauses],
              [processed_parameter.gen_body | acc_gen_body]
            }
        end
      end)

    %{
      keys: keys,
      bin_pieces: Enum.reverse(bin_pieces),
      size: size,
      gen_clauses: gen_clauses,
      gen_body: gen_body
    }
  end

  defp process_parameter(parameter) do
    %{
      key: key(parameter),
      bin_piece: bin_piece(parameter),
      size: parameter.size,
      gen_clause: gen_clause(parameter),
      gen_body: {parameter.name, [], Elixir}
    }
  end

  defp add(x, y) when is_integer(x) and is_integer(y), do: x + y

  defp add(x, y), do: {:+, [context: Elixir, import: Kernel], [x, y]}

  defp bin_piece(%{size: size} = parameters) when is_atom(size) do
    bin_piece(%{parameters | size: {size, [], Elixir}})
  end

  defp bin_piece(%{value: value}), do: value

  defp bin_piece(%{name: name, size: size, type: type}), do: bin_piece(name, size, type)

  defp bin_piece(literal), do: literal

  defp bin_piece(name, size, type) when type in [:op_code, :parameters] do
    {:"::", [],
     [
       {name, [], Elixir},
       {:-, [], [{:binary, [], []}, {:size, [], [size]}]}
     ]}
  end

  defp bin_piece(name, _, type) when type in [:integer, :boolean, :advertising_pdu] do
    {name, [], Elixir}
  end

  defp key(%{name: name, value: value}), do: {name, {value, [], Elixir}}

  defp key(%{name: name}), do: {name, {name, [], Elixir}}

  defp gen_clause(%{value: _}), do: []

  defp gen_clause(%{name: name, size: size}) do
    {:<-, [],
     [
       {name, [], Elixir},
       {{:., [], [{:__aliases__, [alias: false], [:StreamData]}, :binary]}, [], [[length: size]]}
     ]}
  end
end
