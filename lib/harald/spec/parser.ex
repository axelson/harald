defmodule Harald.Spec.Parser do
  @moduledoc false

  require Logger

  def parse(spec) do
    ret =
      spec
      |> Enum.reduce(initial_state(spec), &parse(&1, &2))
      |> Map.take([:ast_maps, :types])

    # IO.inspect("-----------------------------")

    # ret
    # |> Map.get(:ast_maps)
    # |> Enum.at(2)
    # |> Map.get(:deserializers)
    # |> IO.inspect()
    # |> Enum.at(0)
    # |> Macro.to_string()
    # |> Logger.info()

    # IO.inspect("-----------------------------")

    # |> Enum.at(0)
    # |> Macro.to_string()
    # |> Logger.info()

    ret
  end

  def parse({:packets, _spec}, state) do
    # Enum.reduce(spec, state, fn
    #   %{name: name} = packet, state ->
    #     state
    #     |> Map.put(:ast_map, ast_map(:packet, packet, <<2>>))
    #     |> process(state, packet)

    #   _, state ->
    #     state
    # end)
    state
  end

  def parse({:command_groups, spec}, state) do
    Enum.reduce(spec, state, fn command_group, state ->
      parse({:commands, command_group.id, command_group.commands}, state)
    end)
  end

  def parse({:commands, ogf, spec}, state) do
    ret =
      Enum.reduce(spec, state, fn command, state ->
        <<opcode_int::size(16)>> = <<ogf::size(6), command.id::size(10)>>
        opcode = <<opcode_int::little-size(16)>>
        prefix = [1 | :binary.bin_to_list(opcode)]

        state
        |> update_in([:types, :opcode, :values], &[opcode | &1])
        |> put_in([:types, :opcode, :mapping, opcode], command.name)
        |> Map.put(:ast_map, ast_map(:return, command.name, []))
        |> process(spec_unit(command.name, command.return))
        |> Map.put(:ast_map, ast_map(:command, command.name, prefix))
        |> process(command)
      end)

    ret
  end

  def parse({:events, spec}, state) do
    Enum.reduce(spec, state, fn event, state ->
      prefix = [
        4,
        Macro.var(:parameter_total_length, __MODULE__),
        {:"::", [], [Macro.var(:parameters, __MODULE__), Macro.var(:binary, __MODULE__)]}
      ]

      state
      |> Map.put(:ast_map, ast_map(:event, event.name, prefix))
      |> process(event)
    end)
  end

  def parse({_section, _spec}, state) do
    state
  end

  defp initial_state(spec) do
    error_code_ids = Map.keys(Keyword.fetch!(spec, :error_codes))

    %{
      ast_map: nil,
      ast_maps: [],
      spec: spec,
      types: %{
        boolean: %{values: 0..1},
        error_code: %{values: error_code_ids},
        flag: %{size: 2},
        handle: %{size: 12, values: 0..3839},
        integer: %{},
        null_terminated: %{},
        opcode: %{
          size: 16,
          mapping: %{},
          parameters: [%{name: "OGF", size: 6}, %{name: "OCF", size: 10}],
          values: []
        },
        command_return: %{values: %{}}
      }
    }
  end

  defp process(state, %{} = spec_unit) do
    state = process_parameters(state, spec_unit.parameters)
    %{state | ast_map: nil, ast_maps: [state.ast_map | state.ast_maps]}
  end

  defp concat(:serializers, :command, ast, {bin_pieces, transforms, keys}) do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1, [{:serialize, m2, [{:%{}, [], map_args}]}, [{:do, acc_do_value}]]} ->
        do_value = concat_do_value(:serializers, acc_do_value, transforms, bin_pieces)

        {:def, m1,
         [
           {:serialize, m2, [{:%{}, [], map_args ++ keys}]},
           [{:do, do_value}]
         ]}
    end)
  end

  defp concat(:deserializers, :command, ast, {bin_pieces, transforms, keys}) do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1, [{:deserialize, m2, [{:<<>>, [], bin_args}]}, [do: acc_do_value]]} ->
        do_value = concat_do_value(:deserializers, acc_do_value, transforms, keys)
        bin_ast = {:<<>>, [], bin_args ++ bin_pieces}

        {:def, m1,
         [
           {:deserialize, m2, [bin_ast]},
           [do: do_value]
         ]}
    end)
  end

  defp concat(:deserializers, :return, ast, {bin_pieces, transforms, keys}) do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1,
       [{:deserialize, m2, [{{:return, name}, {:<<>>, [], bin_args}}]}, [do: acc_do_value]]} ->
        do_value = concat_do_value(:deserializers, acc_do_value, transforms, keys)
        bin_ast = {:<<>>, [], bin_args ++ bin_pieces}

        {:def, m1,
         [
           {:deserialize, m2, [{{:return, name}, bin_ast}]},
           [do: do_value]
         ]}
    end)
  end

  defp concat_do_value(:event, {:__block__, _, acc_block_args}, new_transforms, new_ret_args) do
    {{a, b, acc_ret_args}, acc_transforms} = List.pop_at(acc_block_args, -1)
    {:__block__, [], acc_transforms ++ new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp concat_do_value(:event, {a, b, acc_ret_args}, [], new_ret_args) do
    {a, b, acc_ret_args ++ new_ret_args}
  end

  defp concat_do_value(:event, {a, b, acc_ret_args}, new_transforms, new_ret_args) do
    {:__block__, [], new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp concat_do_value(section, {:__block__, _, acc_block_args}, new_transforms, new_ret_args) do
    {{a, b, acc_ret_args}, acc_transforms} = List.pop_at(acc_block_args, -1)
    {:__block__, [], acc_transforms ++ new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp concat_do_value(section, {a, b, acc_ret_args}, [], new_ret_args) do
    {a, b, acc_ret_args ++ new_ret_args}
  end

  defp concat_do_value(section, {a, b, acc_ret_args}, new_transforms, new_ret_args) do
    {:__block__, [], new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp wrap_in_block({:__block__, _, _} = ast), do: ast

  defp wrap_in_block(ast), do: {:__block__, [], [ast]}

  defp process_parameters(state, params) do
    Enum.reduce(params, state, fn param, state ->
      expanded_param = expand_parameter(param, state)

      state
      |> process_generators(expanded_param)
      |> process_izers(:deserializers, expanded_param)
      |> process_izers(:serializers, expanded_param)
      |> increment_param_index()
    end)
    |> flatten_izers()
  end

  defp increment_param_index(state) do
    Map.update!(state, :ast_map, fn ast_map ->
      incremented_index = ast_map.parameter_index + 1
      parameter_var = Macro.var(String.to_atom("v#{incremented_index}"), __MODULE__)

      ast_map
      |> Map.put(:parameter_index, incremented_index)
      |> Map.put(:parameter_var, parameter_var)
    end)
  end

  defp flatten_izers(state) do
    state
    |> update_in([:ast_map, :deserializers], &List.flatten/1)
    |> update_in([:ast_map, :serializers], &List.flatten/1)
  end

  defp process_generators(state, param) do
    generator = generator_chunk(param, state)

    state
    |> update_in([:ast_map, :generators], fn generators ->
      Enum.map(generators, fn ast ->
        Macro.prewalk(ast, fn
          {:gen, [], [{:all, [], clauses}, [do: {:<<>>, [], args}]]} ->
            clauses = clauses ++ generator.gen_clauses
            args = args ++ generator.gen_body
            {:gen, [], [{:all, [], clauses}, [do: {:<<>>, [], args}]]}

          ast ->
            ast
        end)
      end)
    end)
  end

  defp process_izers(state, izer_type, param) when izer_type in [:serializers, :deserializers] do
    update_in(state, [:ast_map, izer_type], fn [acc_head | acc_tail] ->
      head = izer_chunks(param, izer_type, acc_head, state)
      [head | acc_tail]
    end)
  end

  defp expand_parameter(param, acc) do
    case param[:type] do
      :command_return ->
        param

      _ ->
        type_id = Map.get(param, :type, :integer)
        type = resolve_type(type_id, acc)
        type_size = Map.get(type, :size, 8)

        param =
          param
          |> Map.put_new(:type, type_id)
          |> Map.put_new(:size, type_size)

        case Map.fetch(type, :values) do
          {:ok, values} -> Map.put_new(param, :values, values)
          _ -> param
        end
    end
  end

  defp resolve_type({:branch, type_id}, acc), do: resolve_type(type_id, acc)
  defp resolve_type(type_id, acc), do: Map.fetch!(acc.types, type_id)

  defp izer_chunks(param, izer_type, acc_izer, state)

  defp izer_chunks(%{type: _type} = param, izer_type, acc_izer, state) do
    bin_pieces = bin_piece(param, state)
    transforms = transforms(param, izer_type, state)
    keys = map_key(param, izer_type, state)
    concat(acc_izer, {bin_pieces, transforms, keys})
  end

  defp transforms(param, izer_type, state)

  defp transforms(%{type: :error_code}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.error_code(unquote(parameter_var))
      end
    ]
  end

  defp transforms(%{type: :error_code}, :deserializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.error_desc(unquote(parameter_var))
      end
    ]
  end

  defp transforms(%{type: :command_return}, :deserializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.deserialize(unquote(parameter_var))
      end
    ]
  end

  defp transforms(_, _, _), do: []

  defp add(x, y) when is_integer(x) and is_integer(y), do: x + y
  defp add(x, y) when is_atom(x), do: add({x, [], nil}, y)
  defp add(x, y) when is_atom(y), do: add(x, {y, [], nil})
  defp add(x, y), do: {:+, [context: nil, import: Kernel], [x, y]}

  defp bin_piece(%{type: :command_return}, state) do
    # TODO use quote
    [{:"::", [], [state.ast_map.parameter_var, Macro.var(:binary, __MODULE__)]}]
  end

  defp bin_piece(%{size: size} = param, state) when is_atom(size) do
    bin_piece(%{param | size: state.ast_map.parameter_var}, state)
  end

  defp bin_piece(%{type: :null_terminated} = param, state) do
    parameter_var = state.ast_map.parameter_var
    size = div(param.size, 8)

    quote do
      <<unquote(parameter_var)::binary-size(unquote(size))>>
    end
    |> elem(2)
  end

  defp bin_piece(%{value: value}, _), do: [value]

  defp bin_piece(%{size: 8, type: :error_code}, state) do
    [state.ast_map.parameter_var]
  end

  defp bin_piece(%{size: size, type: _type}, state) do
    [{:"::", [], [state.ast_map.parameter_var, {:size, [], [size]}]}]
  end

  # defp transforms(%{type: type} = param, acc) when type in [:opcode, {:branch, :opcode}] do
  #   command_name = Map.fetch!(acc.types.opcode.mapping, param.value)
  #   name = Macro.var(param.name, __MODULE__)

  #   [
  #     quote do
  #       var!(unquote(name)) = unquote(command_name)
  #     end
  #   ]
  # end

  # defp transforms(%{type: :null_terminated} = param, acc) do
  #   name = Macro.var(param.name, __MODULE__)

  #   [
  #     quote do
  #       [var!(unquote(name)) | _] = String.split(var!(unquote(name)), <<0>>, parts: 2)
  #     end
  #   ]
  # end

  # defp transforms(%{type: {:branch, :error_codes}} = param, acc) do
  #   error_code_name =
  #     acc.spec
  #     |> Keyword.fetch!(:error_codes)
  #     |> Map.fetch!(param.value)

  #   name = Macro.var(param.name, __MODULE__)

  #   [
  #     quote do
  #       var!(unquote(name)) = unquote(error_code_name)
  #     end
  #   ]
  # end

  # defp transforms(%{type: :boolean} = param, acc) do
  #   var = Macro.var(param.name, __MODULE__)

  #   [
  #     quote do
  #       var!(unquote(var)) =
  #         case unquote(var) do
  #           0 -> false
  #           false -> 0
  #           1 -> true
  #           true -> 1
  #         end
  #     end
  #   ]
  # end

  # defp transforms(param, _), do: []

  defp map_key(%{type: {:branch, type_id}} = param, :serializers, state)
       when type_id in [:opcode, :error_code] do
    [{param.name, state.ast_map.parameter_var}]
  end

  defp map_key(%{value: value} = param, :serializers, state) do
    IO.inspect(:suspicious)
    [{param.name, quote(do: unquote(value) = state.ast_map.parameter_var)}]
  end

  defp map_key(param, :serializers, state), do: [{param.name, state.ast_map.parameter_var}]

  defp map_key(param, :deserializers, state), do: [{param.name, state.ast_map.parameter_var}]

  # defp des_key(%{name: name, type: {:branch, type_id}}) when type_id in [:opcode, :error_code] do
  #   [{name, {name, [], nil}}]
  # end

  # defp des_key(%{name: name, value: value}) do
  #   name = Macro.var(name, __MODULE__)
  #   [{name, quote(do: unquote(value) = name)}]
  # end

  # defp des_key(%{name: name}), do: [{name, {name, [], nil}}]

  defp generator_chunk(param, state) do
    %{gen_body: gen_body(param, state), gen_clauses: gen_clauses(param, state)}
  end

  defp gen_clauses(param, state)

  defp gen_clauses(%{value: _}, _), do: []

  defp gen_clauses(%{type: :command_return}, state) do
    opcode = generator_look_behind(1, state)
    command_name = get_in(state, [:types, :opcode, :mapping, opcode])
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- Harald.Generators.HCI.generate({:return, unquote(command_name)})
      end
    ]
  end

  defp gen_clauses(%{values: values}, state) do
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- StreamData.member_of(unquote(values))
      end
    ]
  end

  defp gen_clauses(%{type: :integer}, state) do
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- StreamData.integer(0..255)
      end
    ]
  end

  defp gen_clauses(%{type: :error_code}, state) do
    error_codes = state.types.error_codes.values
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- StreamData.member_of(unquote(error_codes))
      end
    ]
  end

  defp gen_clauses(%{type: :null_terminated} = param, state) do
    size = div(param.size, 8)
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- Harald.Generators.HCI.generate(:null_terminated, length: unquote(size))
      end
    ]
  end

  defp look_behind(_izer_type, acc_izer, depth) do
    {:def, _meta, [head, _body]} = acc_izer
    {_, _, [{:<<>>, _, bin_args}]} = head

    bin_args
    |> Enum.at(-depth)
  end

  defp generator_look_behind(depth, state) do
    [generator] = state.ast_map.generators

    generator
    |> gen_clauses_from_generator()
    |> Enum.at(-depth)
    |> elem(2)
    |> Enum.at(-1)
    |> elem(2)
    |> Enum.at(0)
    |> Enum.at(0)
  end

  defp gen_clauses_from_generator(ast) do
    {_, acc} =
      Macro.prewalk(ast, nil, fn
        ast, acc when not is_nil(acc) ->
          {ast, acc}

        {:all, _, gen_clauses} = ast, nil ->
          {ast, gen_clauses}

        ast, acc ->
          {ast, acc}
      end)

    acc
  end

  defp gen_body(%{type: {:branch, type_id}} = param, state) do
    gen_body(Map.put(param, :type, type_id), state)
  end

  defp gen_body(%{value: value}, _) do
    [Macro.escape(value)]
  end

  defp gen_body(%{type: type}, state) when type in [:command_return, :null_terminated, :opcode] do
    var = state.ast_map.parameter_var

    [
      quote do
        unquote(var) :: binary
      end
    ]
  end

  defp gen_body(_, state) do
    [state.ast_map.parameter_var]
  end

  defp merge_map(new, old) do
    Enum.into(old, %{}, fn {k, v_old} ->
      v_new = Map.fetch!(new, k)

      cond do
        is_list(v_old) -> {k, v_old ++ List.wrap(v_new)}
        k == :name -> {k, v_old}
        true -> {k, add(v_old, v_new)}
      end
    end)
  end

  defp ast_map(type, name, prefix) do
    %{
      deserializers: [
        quote do
          def deserialize(
                unquote(ast_map_parameter(type, :deserializers, name, {:<<>>, [], prefix}))
              ) do
            unquote(ast_map_return(type, name))
          end
        end
      ],
      generators: [
        quote do
          def generate(unquote(ast_map_name(type, :generators, name))) do
            gen all(bin <- StreamData.constant(unquote({:<<>>, [], prefix}))) do
              <<bin::binary>>
            end
          end
        end
      ],
      parameter_var: Macro.var(:v1, __MODULE__),
      parameter_index: 1,
      serializers: [
        [
          quote do
            def serialize(%{type: unquote(type), name: unquote(name)}) do
              parameters = <<>>
              parameter_total_length = byte_size(parameters)
              unquote({:<<>>, [], prefix})
            end
          end
        ]
      ]
    }
  end

  defp ast_map_parameter(:return = type, section, name, ast) do
    {ast_map_name(type, section, name), ast}
  end

  defp ast_map_parameter(type, :deserializers, name, {:<<>>, [], bin_args})
       when type in [:event, :command] do
    {:<<>>, [], bin_args ++ [Macro.var(:_size, __MODULE__)]}
  end

  defp ast_map_parameter(_, _, _, ast), do: ast

  defp ast_map_name(:return = type, :generators, name), do: {type, name}

  defp ast_map_name(:return = type, section, name) do
    {type, {:=, [], [name, {:name, [], __MODULE__}]}}
  end

  defp ast_map_name(_, section, name), do: name

  defp ast_map_return(:return = type, _) do
    quote do
      %{type: unquote(type), name: name}
    end
  end

  defp ast_map_return(type, name) do
    quote do
      %{type: unquote(type), name: unquote(name)}
    end
  end

  defp ast_map_deserializers(type, name, prefix) do
    [
      quote do
        def deserialize(unquote({:<<>>, [], prefix})) do
          %{type: unquote(type), name: unquote(name)}
        end
      end
    ]
  end

  defp spec_unit(name, parameters), do: %{name: name, parameters: parameters}
end
