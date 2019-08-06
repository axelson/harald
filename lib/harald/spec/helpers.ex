defmodule Harald.Spec.Helpers do
  @moduledoc false

  use ExUnitProperties

  @doc false
  def define_generators(processed_spec) do
    %{ast: core_ast, commands: commands, events: events} =
      Enum.reduce(processed_spec, %{ast: [], commands: [], events: []}, fn
        {:packets, _}, acc ->
          acc

        {key, value}, acc when key in [:commands, :events] ->
          Enum.reduce(value, acc, fn
            %{
              gen_body: gen_body,
              gen_clauses: gen_clauses,
              name: name
            },
            acc ->
              ast =
                quote do
                  def generate(unquote(name)) do
                    gen all(unquote_splicing(gen_clauses)) do
                      unquote(gen_body)
                    end
                  end
                end

              %{
                acc
                | :ast => acc.ast ++ [ast],
                  key => [name | acc[key]]
              }
          end)
      end)

    extra_ast = [
      quote do
        def generate(:command) do
          gen all(
                command <- StreamData.member_of(unquote(commands)),
                bin <- generate(command)
              ) do
            bin
          end
        end

        def generate(:event) do
          gen all(
                event <- StreamData.member_of(unquote(events)),
                bin <- generate(event)
              ) do
            bin
          end
        end
      end
    ]

    core_ast ++ extra_ast
  end

  @doc false
  def define_serializers(processed_spec) do
    Enum.reduce(processed_spec, [], fn
      {:packets, _}, acc ->
        acc

      {key, value}, acc when key in [:commands, :events] ->
        Enum.reduce(value, acc, fn
          %{
            bin_body: bin_body,
            bin_pattern: bin_pattern,
            parameters: parameters
          },
          acc ->
            ast =
              quote generated: true do
                def serialize(unquote(parameters)) do
                  {:ok, unquote(bin_body)}
                end

                def serialize(x), do: {:error, x}

                def deserialize(unquote(bin_pattern)) do
                  {:ok, unquote(parameters)}
                end

                def deserialize(x), do: {:error, x}
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

  defp process_command_groups(command_groups) do
    Enum.reduce(command_groups, [], fn {ogf, commands}, acc ->
      acc ++ process_commands(ogf, commands)
    end)
  end

  defp process_commands(ogf, commands) do
    Enum.reduce(commands, [], fn command, acc -> [process_command(ogf, command) | acc] end)
  end

  defp process_command(ogf, command) do
    processed_parameters = process_parameters(command.parameters)

    gen_clause =
      {:<-, [], [{:ocf, [], Elixir}, quote(do: StreamData.constant(unquote(command.ocf)))]}

    command_name = command.command
    keys = [{:command, command_name} | processed_parameters.keys]
    <<opcode::size(16)>> = <<ogf::size(6), command.ocf::size(10)>>
    opcode = <<opcode::little-size(16)>>

    gen_body =
      {:<<>>, [], [1, opcode, div(processed_parameters.size, 8) | processed_parameters.gen_body]}

    bin_body =
      {:<<>>, [],
       [
         1,
         opcode,
         div(processed_parameters.size, 8) | processed_parameters.bin_pieces
       ]}

    bin_pattern = {:<<>>, [], [1, opcode, {:size, [], Elixir} | processed_parameters.bin_pieces]}

    Map.merge(
      Map.drop(processed_parameters, [:bin_pieces]),
      %{
        bin_body: bin_body,
        bin_pattern: bin_pattern,
        keys: keys,
        name: command_name,
        parameters: {:%{}, [], keys},
        gen_body: gen_body,
        gen_clauses: [gen_clause | processed_parameters.gen_clauses]
      }
    )
  end

  defp process_events(events) do
    Enum.reduce(events, [], fn
      %{subevents: subevents}, acc -> process_events(subevents) ++ acc
      event, acc -> [process_event(event) | acc]
    end)
  end

  defp process_event(event) do
    processed_parameters = process_parameters(event.parameters)
    event_code = event_code(event)

    gen_clause =
      {:<-, [], [{:event_code, [], Elixir}, quote(do: StreamData.constant(unquote(event_code)))]}

    event_name = event.event
    keys = [{:event, event_name} | processed_parameters.keys]

    gen_body =
      {:<<>>, [],
       [4, event_code, div(processed_parameters.size, 8) | processed_parameters.gen_body]}

    bin_body =
      {:<<>>, [],
       [4, event_code, div(processed_parameters.size, 8) | processed_parameters.bin_pieces]}

    bin_pattern =
      {:<<>>, [], [4, event_code, {:size, [], Elixir} | processed_parameters.bin_pieces]}

    Map.merge(
      Map.drop(processed_parameters, [:bin_pieces]),
      %{
        bin_body: bin_body,
        bin_pattern: bin_pattern,
        keys: keys,
        name: event_name,
        parameters: {:%{}, [], keys},
        gen_body: gen_body,
        gen_clauses: [gen_clause | processed_parameters.gen_clauses]
      }
    )
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
        case parameter.type do
          :arrayed_data ->
            processed_parameter = process_parameters(parameter.parameters)

            {
              processed_parameter.keys ++ acc_keys,
              acc_bin_pieces ++ processed_parameter.bin_pieces,
              add(processed_parameter.size, acc_size),
              acc_gen_clauses ++ processed_parameter.gen_clauses,
              processed_parameter.gen_body ++ acc_gen_body
            }

          _ ->
            processed_parameter = process_parameter(parameter)

            {
              [processed_parameter.key | acc_keys],
              acc_bin_pieces ++ [processed_parameter.bin_piece],
              add(processed_parameter.size, acc_size),
              acc_gen_clauses ++ [processed_parameter.gen_clause],
              [processed_parameter.gen_body | acc_gen_body]
            }
        end
      end)

    %{
      keys: keys,
      bin_pieces: bin_pieces,
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
      gen_body: gen_body(parameter)
    }
  end

  defp add(x, y) when is_integer(x) and is_integer(y), do: x + y

  defp add(x, y) when is_atom(x), do: add({x, [], Elixir}, y)
  defp add(x, y) when is_atom(y), do: add(x, {y, [], Elixir})
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

  defp key(%{name: name, value: value}), do: {name, value}
  defp key(%{name: name}), do: {name, {name, [], Elixir}}

  defp gen_body(%{value: value}), do: value
  defp gen_body(parameter), do: {:"::", [], [{parameter.name, [], Elixir}, {:binary, [], Elixir}]}

  defp gen_clause(%{value: _}), do: []

  defp gen_clause(%{name: name, size: size}) do
    size = if is_atom(size), do: {size, [], Elixir}, else: size

    {:<-, [],
     [
       {name, [], Elixir},
       quote(do: StreamData.binary(length: unquote(div(size, 8))))
     ]}
  end
end
