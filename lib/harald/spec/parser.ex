defmodule Harald.Spec.Parser do
  @doc false

  def parse(spec) do
    error_code_ids = Map.keys(Keyword.fetch!(spec, :error_codes))

    acc = %{
      branches: %{
        error_codes: %{type: :error_code, values: error_code_ids},
        op_codes: %{type: :op_code, values: []}
      },
      gen_maps: %{commands: [], events: [], packets: []},
      parameters: %{returns: %{}},
      spec: spec,
      types: %{
        # rfu, error_code, and return_parameters are all special types that have no definition here
        # but exist for semantic reasons.
        error_code: %{values: error_code_ids},
        handle: %{size: 12, values: 0..3839},
        op_code: %{size: 16, parameters: [%{name: "OGF", size: 6}, %{name: "OCF", size: 10}]},
        flag: %{size: 2}
      }
    }

    Enum.reduce(spec, acc, &parse(&1, &2))
  end

  def parse({:packets, spec}, acc) do
    Enum.reduce(spec, acc, fn
      %{name: :acl_data} = packet, acc -> parse({:packets, :packet, packet.id, packet}, acc)
      _, acc -> acc
    end)
  end

  def parse({:command_groups, spec}, acc) do
    Enum.reduce(spec, acc, fn command_group, acc ->
      parse({:commands, command_group.id, command_group.commands}, acc)
    end)
  end

  def parse({:commands, ogf, spec}, acc) do
    Enum.reduce(spec, acc, fn command, acc ->
      <<op_code_int::size(16)>> = <<ogf::size(6), command.id::size(10)>>
      op_code = <<op_code_int::little-size(16)>>

      acc =
        acc
        |> update_in([:branches, :op_codes, :values], &[op_code | &1])
        |> put_in([:parameters, :returns, op_code], parse_parameters(command.return, acc))

      parse({:commands, :command, op_code, command}, acc)
    end)
  end

  def parse({:events, spec}, acc) do
    Enum.reduce(spec, acc, fn
      event, acc ->
        parse({:events, :event, event.id, event}, acc)
    end)
  end

  def parse({_section, _spec}, acc), do: acc

  def parse({parent_section, section, id, spec}, acc)
      when section in [:command, :event, :packet] do
    gen_maps =
      spec.parameters
      |> parse_parameters(acc)
      |> List.wrap()
      |> Enum.map(fn {keys, bin_pieces, size, gen_clauses, gen_body} ->
        gen_clause = {:<-, [], [{:id, [], Elixir}, quote(do: constant(unquote(id)))]}
        keys = [{section, spec.name} | keys]

        packet_indicator =
          case parent_section do
            :packets -> <<>>
            :commands -> 1
            :events -> 4
          end

        size = div(size, 8)
        gen_body = {:<<>>, [], [id, size | gen_body]}
        bin_body = {:<<>>, [], [id, size | bin_pieces]}
        bin_pattern = {:<<>>, [], [id, size | bin_pieces]}

        %{
          bin_body: bin_body,
          bin_pattern: bin_pattern,
          gen_body: gen_body,
          gen_clauses: [gen_clause | gen_clauses],
          keys: keys,
          name: spec.name,
          parameters: {:%{}, [], keys}
          # size: size
        }
      end)
      |> Enum.concat(Map.fetch!(acc.gen_maps, parent_section))

    put_in(acc, [:gen_maps, parent_section], gen_maps)
  end

  defp parse_parameters(params, acc) do
    params
    |> Enum.reduce([{[], [], 0, [], []}], fn
      param, inner_acc ->
        inner_acc
        |> Enum.map(fn {keys, bin_pieces, size, gen_clauses, gen_body} ->
          case param[:type] do
            :arrayed_data ->
              raise "taco bell"
              processed_parameter = parse_parameters(param.parameters, acc)

              {
                processed_parameter.keys ++ keys,
                bin_pieces ++ processed_parameter.bin_pieces,
                add(processed_parameter.size, size),
                gen_clauses ++ processed_parameter.gen_clauses,
                processed_parameter.gen_body ++ gen_body
              }

            {:branch, branch} ->
              %{type: branch_type, values: values} = Map.fetch!(acc.branches, branch)

              branch_type_size =
                acc.types
                |> Map.fetch!(branch_type)
                |> Map.get(:size, 8)

              Enum.map(values, fn branch_value ->
                param =
                  param
                  |> IO.inspect()
                  |> Map.put_new(:size, branch_type_size)
                  |> Map.put(:value, branch_value)

                {
                  [key(param) | keys],
                  bin_pieces ++ [bin_piece(param, acc)],
                  add(param.size, size),
                  gen_clauses ++ [gen_clause(param, acc)],
                  gen_body ++ [gen_body(param)]
                }
              end)

            {:parameters, path} ->
              path =
                Enum.map(path, fn
                  {:look_behind, depth} ->
                    inner_acc
                    |> Enum.at(0)
                    |> elem(0)
                    |> Enum.at(depth - 1)
                    |> elem(1)

                  key ->
                    key
                end)

              acc.parameters
              |> get_in(path)
              |> Enum.map(fn {p_keys, p_bin_pieces, p_size, p_gen_clauses, p_gen_body} ->
                {
                  keys ++ p_keys,
                  bin_pieces ++ p_bin_pieces,
                  add(p_size, size),
                  gen_clauses ++ p_gen_clauses,
                  gen_body ++ p_gen_body
                }
              end)

            type ->
              type_size = get_in(acc, [:types, type, :size]) || 8
              param = Map.put_new(param, :size, type_size)

              {
                [key(param) | keys],
                bin_pieces ++ [bin_piece(param, acc)],
                add(param.size, size),
                gen_clauses ++ [gen_clause(param, acc)],
                gen_body ++ [gen_body(param)]
              }
          end
        end)
        |> List.flatten()
    end)
  end

  defp add(x, y) when is_integer(x) and is_integer(y), do: x + y
  defp add(x, y) when is_atom(x), do: add({x, [], Elixir}, y)
  defp add(x, y) when is_atom(y), do: add(x, {y, [], Elixir})
  defp add(x, y), do: {:+, [context: Elixir, import: Kernel], [x, y]}

  defp bin_piece(%{size: size} = param, acc) when is_atom(size) do
    bin_piece(%{param | size: {size, [], Elixir}}, acc)
  end

  defp bin_piece(%{value: value}, _), do: value

  defp bin_piece(%{name: name, size: size, type: type}, acc) do
    {:"::", [], [{name, [], Elixir}, {:-, [], [{:bits, [], []}, {:size, [], [size]}]}]}
  end

  defp bin_piece(%{name: name} = param, _), do: {name, [], Elixir}

  defp key(%{name: name, value: value}), do: {name, value}
  defp key(%{name: name}), do: {name, {name, [], Elixir}}

  defp gen_clause(%{value: _}, acc), do: []

  defp gen_clause(%{name: name, type: type} = param, acc) do
    size = if is_atom(param.size), do: {param.size, [], Elixir}, else: param.size

    value =
      case type do
        :error_code ->
          quote do: member_of(Map.keys(acc.spec.error_codes))

        :null_terminated ->
          quote bind_quoted: [size: div(size, 8)] do
            :printable
            |> string()
            |> filter(fn x -> byte_size(x) <= size end)
            |> map(fn x ->
              case size - byte_size(x) do
                0 -> x
                space -> x <> <<0>> <> Enum.at(binary(length: space - 1), 0)
              end
            end)
          end
      end

    {:<-, [], [{name, [], Elixir}, value]}
  end

  defp gen_clause(%{name: name} = param, acc) do
    value = quote bind_quoted: [size: param.size], do: binary(length: div(size, 8))
    {:<-, [], [{name, [], Elixir}, value]}
  end

  defp gen_body(%{value: value}), do: value

  defp gen_body(param) do
    {:"::", [], [{param.name, [], Elixir}, {:binary, [], Elixir}]}
  end
end
