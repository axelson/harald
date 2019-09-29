defmodule Harald.Spec.Parser do
  @moduledoc false

  require Logger

  def parse(spec) do
    ret =
      spec
      |> Enum.reduce(initial_state(spec), &parse(&1, &2))
      |> generate_helpers()
      |> Map.take([:ast_maps, :helpers, :types])

    ret.ast_maps
    |> Enum.at(0)
    |> Map.get(:deserializers)
    |> Macro.to_string()

    # |> Logger.warn()

    ret
  end

  def parse({:command_groups, spec}, state) do
    Enum.reduce(spec, state, fn command_group, state ->
      parse({:commands, command_group.id, command_group.commands}, state)
    end)
  end

  def parse({:commands, ogf, spec}, state) do
    Enum.reduce(spec, state, fn command, state ->
      <<opcode_int::size(16)>> = <<ogf::size(6), command.id::size(10)>>
      opcode = <<opcode_int::little-size(16)>>
      prefix = [1 | :binary.bin_to_list(opcode)]

      state
      |> update_in([:types, :opcode, :values], &[opcode | &1])
      |> put_in([:types, :opcode, :mapping, opcode], command.name)
      |> Map.put(:ast_map, ast_map(:return, command.name, []))
      |> process_parameters(:return, spec_unit(command.name, command.return).parameters)
      |> Map.put(:ast_map, ast_map(:command, command.name, prefix))
      |> process_parameters(:command, command.parameters)
    end)
  end

  def parse({:events, spec}, state) do
    Enum.reduce(spec, state, fn
      %{subevents: subevents} = event, state ->
        parse({:subevents, {event.id, event.name}, subevents}, state)

      event, state ->
        prefix = [4, event.id]

        state
        |> Map.put(:ast_map, ast_map(:event, event.name, prefix))
        |> process_parameters(:event, event.parameters)
    end)
  end

  def parse({:subevents, {event_id, event_name}, spec}, state) do
    prefix = [4, event_id]

    Enum.reduce(spec, state, fn
      subevent, state ->
        state
        |> Map.put(:ast_map, ast_map(:subevent, {event_name, subevent.name}, prefix))
        |> process_parameters(:subevent, subevent.parameters)
    end)
  end

  def parse({_section, _spec}, state), do: state

  defp generate_helpers(state) do
    command_name_funs =
      Enum.reduce(state.types.opcode.mapping, [], fn {opcode, command_name}, acc ->
        <<int_opcode::size(16)>> = opcode

        ast =
          quote do
            def command_name(unquote(opcode)), do: unquote(command_name)
            def command_name(unquote(int_opcode)), do: unquote(command_name)
            def command_opcode(unquote(command_name)), do: unquote(opcode)
          end

        [ast | acc]
      end)

    error_funs =
      Enum.reduce(Keyword.fetch!(state.spec, :error_codes), [], fn {error_code, error_desc},
                                                                   acc ->
        ast =
          quote do
            def error_code(unquote(error_desc)), do: unquote(error_code)
            def error_desc(unquote(error_code)), do: unquote(error_desc)
          end

        [ast | acc]
      end)

    helpers = command_name_funs ++ error_funs
    Map.put(state, :helpers, helpers)
  end

  defp initial_state(spec) do
    error_code_ids = Map.keys(Keyword.fetch!(spec, :error_codes))

    %{
      ast_map: nil,
      ast_maps: [],
      spec: spec,
      types: %{
        arrayed_data: %{},
        boolean: %{values: 0..1},
        command_return: %{values: %{}},
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
        subevent_code: %{mapping: %{}}
      }
    }
  end

  defp concat(:serializers, :command, ast, {bin_pieces, transforms, keys}) do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1, [{:serialize, m2, [{:%{}, [], map_args}]}, [{:do, acc_do_value}]]} ->
        do_value = concat_do_value(acc_do_value, transforms, bin_pieces)

        {:def, m1,
         [
           {:serialize, m2, [{:%{}, [], map_args ++ keys}]},
           [{:do, do_value}]
         ]}
    end)
  end

  defp concat(:serializers, :return, ast, {bin_pieces, transforms, keys}) do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1, [{:serialize, m2, [{:%{}, [], map_args}]}, [{:do, acc_do_value}]]} ->
        do_value = concat_do_value(acc_do_value, transforms, bin_pieces)

        {:def, m1,
         [
           {:serialize, m2, [{:%{}, [], map_args ++ keys}]},
           [{:do, do_value}]
         ]}
    end)
  end

  defp concat(:serializers, spec_type, ast, {new_bin_args, transforms, keys})
       when spec_type in [:event, :subevent] do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1,
       [{:serialize, m2, [{:%{}, [], map_args}]}, [{:do, {:__block__, _, acc_block_args}}]]} ->
        {acc_transforms,
         [
           {:=, [], [parameters_var, {:<<>>, [], acc_bin_args}]},
           parameter_total_length,
           ret_bin
         ]} = Enum.split(acc_block_args, -3)

        bin_args = acc_bin_args ++ new_bin_args

        do_value =
          {:__block__, [],
           acc_transforms ++
             transforms ++
             [{:=, [], [parameters_var, {:<<>>, [], bin_args}]}, parameter_total_length, ret_bin]}

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
        do_value = concat_do_value(acc_do_value, transforms, keys)
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
        do_value = concat_do_value(acc_do_value, transforms, keys)
        bin_ast = {:<<>>, [], bin_args ++ bin_pieces}

        {:def, m1,
         [
           {:deserialize, m2, [{{:return, name}, bin_ast}]},
           [do: do_value]
         ]}
    end)
  end

  defp concat(:deserializers, spec_type, ast, {bin_pieces, transforms, keys})
       when spec_type in [:event, :subevent] do
    ast
    |> List.wrap()
    |> Enum.map(fn
      {:def, m1, [{:deserialize, m2, [{:<<>>, [], bin_args}]}, [do: acc_do_value]]} ->
        do_value = concat_do_value(acc_do_value, transforms, keys)
        bin_ast = {:<<>>, [], bin_args ++ bin_pieces}

        {:def, m1,
         [
           {:deserialize, m2, [bin_ast]},
           [do: do_value]
         ]}
    end)
  end

  defp concat_do_value({:__block__, _, acc_block_args}, new_transforms, new_ret_args) do
    {{a, b, acc_ret_args}, acc_transforms} = List.pop_at(acc_block_args, -1)
    {:__block__, [], acc_transforms ++ new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp concat_do_value({a, b, acc_ret_args}, [], new_ret_args) do
    {a, b, acc_ret_args ++ new_ret_args}
  end

  defp concat_do_value({a, b, acc_ret_args}, new_transforms, new_ret_args) do
    {:__block__, [], new_transforms ++ [{a, b, acc_ret_args ++ new_ret_args}]}
  end

  defp wrap_in_block({:__block__, _, _} = ast), do: ast

  defp wrap_in_block(ast), do: {:__block__, [], [ast]}

  defp process_parameters(state, spec_type, params) do
    state =
      params
      |> Enum.reduce(state, fn
        param, state ->
          expanded_param = expand_parameter(param, state)

          state
          |> process_generators(spec_type, expanded_param)
          |> process_izers(:deserializers, spec_type, expanded_param)
          |> process_izers(:serializers, spec_type, expanded_param)
          |> increment_param_index()
      end)
      |> parameters_finish()

    state
    |> Map.put(:ast_map, nil)
    |> Map.update!(:ast_maps, &[state.ast_map | &1])
  end

  defp process_parameters(state, spec_type, params) do
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

  defp parameters_finish(state) do
    state
    |> update_in([:ast_map, :deserializers], &List.flatten/1)
    |> update_in([:ast_map, :serializers], &List.flatten/1)
  end

  defp process_generators(state, spec_type, param)

  defp process_generators(state, spec_type, param) when spec_type in [:event, :subevent] do
    generator = generator_chunk(param, state)

    state
    |> update_in([:ast_map, :generators], fn generators ->
      Enum.map(generators, fn
        {:def, m1, [head, [do: {:gen, [], [{:all, [], acc_clauses}, body]}]]} = ast ->
          {clauses_head,
           [
             {:=, [], [parameters, {:<<>>, [], acc_bin_args}]},
             parameter_total_length
           ]} = Enum.split(acc_clauses, -2)

          clauses_head = clauses_head ++ generator.gen_clauses
          bin_args = acc_bin_args ++ generator.gen_body

          clauses =
            clauses_head ++
              [{:=, [], [parameters, {:<<>>, [], bin_args}]}, parameter_total_length]

          {:def, m1, [head, [do: {:gen, [], [{:all, [], clauses}, body]}]]}
      end)
    end)
  end

  defp process_generators(state, _spec_type, param) do
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

  defp process_izers(state, ast_type, spec_type, param)
       when ast_type in [:serializers, :deserializers] do
    update_in(state, [:ast_map, ast_type], fn [acc_head | acc_tail] ->
      head = izer_chunks(param, ast_type, spec_type, acc_head, state)
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

  defp resolve_type(type_id, acc), do: Map.fetch!(acc.types, type_id)

  defp izer_chunks(param, ast_type, spec_type, acc_izer, state)

  defp izer_chunks(%{type: _type} = param, ast_type, spec_type, acc_izer, state) do
    bin_pieces = bin_piece(param, state)
    transforms = transforms(param, ast_type, state)
    keys = map_key(param, ast_type, state)
    concat(ast_type, spec_type, acc_izer, {bin_pieces, transforms, keys})
  end

  defp transforms(param, ast_type, state)

  defp transforms(%{type: :boolean}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.serialize({:boolean, unquote(parameter_var)})
      end
    ]
  end

  defp transforms(%{type: :error_code}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.error_code(unquote(parameter_var))
      end
    ]
  end

  defp transforms(%{type: :command_return}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.serialize(unquote(parameter_var))
      end
    ]
  end

  defp transforms(%{type: :null_terminated}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) =
          elem(unquote(parameter_var), 0) <> <<0>> <> elem(unquote(parameter_var), 1)
      end
    ]
  end

  defp transforms(%{type: :opcode}, :serializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.command_opcode(unquote(parameter_var))
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
    command_opcode = relative_parameter(-1, state)

    [
      quote do
        unquote(parameter_var) =
          Harald.HCI.deserialize({{:return, unquote(command_opcode)}, unquote(parameter_var)})
      end
    ]
  end

  defp transforms(%{type: :null_terminated}, :deserializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        [head | tail] = String.split(unquote(parameter_var), <<0>>)
      end,
      quote do
        unquote(parameter_var) = {head, Enum.join(tail)}
      end
    ]
  end

  defp transforms(%{type: :opcode}, :deserializers, state) do
    parameter_var = state.ast_map.parameter_var

    [
      quote do
        unquote(parameter_var) = Harald.HCI.command_name(unquote(parameter_var))
      end
    ]
  end

  defp relative_parameter(offset, state) do
    "v#{state.ast_map.parameter_index + offset}"
    |> String.to_atom()
    |> Macro.var(__MODULE__)
  end

  defp transforms(_, _, _), do: []

  defp add(x, y) when is_integer(x) and is_integer(y), do: x + y
  defp add(x, y) when is_atom(x), do: add({x, [], nil}, y)
  defp add(x, y) when is_atom(y), do: add(x, {y, [], nil})
  defp add(x, y), do: {:+, [context: nil, import: Kernel], [x, y]}

  defp bin_piece(%{value: value}, _), do: [value]

  defp bin_piece(%{type: type}, state) when type in [:arrayed_data, :command_return] do
    # TODO use quote
    [{:"::", [], [state.ast_map.parameter_var, Macro.var(:binary, __MODULE__)]}]
  end

  defp bin_piece(%{type: :opcode}, state) do
    parameter_var = state.ast_map.parameter_var

    quote do
      <<unquote(parameter_var)::binary-size(2)>>
    end
    |> elem(2)
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

  defp bin_piece(%{size: 8, type: :error_code}, state) do
    [state.ast_map.parameter_var]
  end

  defp bin_piece(%{size: size, type: _type} = param, state) do
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

  # defp map_key(%{type: {:branch, type_id}} = param, :serializers, state)
  #      when type_id in [:opcode, :error_code] do
  #   [{param.name, state.ast_map.parameter_var}]
  # end

  # defp map_key(%{value: value} = param, :serializers, state) do
  #   IO.inspect(param)
  #   raise "suspicious"
  #   [{param.name, quote(do: unquote(value) = state.ast_map.parameter_var)}]
  # end

  defp map_key(%{type: :subevent_code} = param, _, state) do
    name = param.name
    subevent_name = state.ast_map.subevent_name

    [
      quote do
        {unquote(name), unquote(subevent_name)}
      end
    ]
  end

  defp map_key(param, :deserializers, state), do: [{param.name, state.ast_map.parameter_var}]

  defp map_key(param, :serializers, state), do: [{param.name, state.ast_map.parameter_var}]

  # defp map_key(%{type: :subevent_code} = param, :serializers, state) do
  #   name = param.name
  #   subevent_name = state.ast_map.subevent_name

  #   [
  #     quote do
  #       {unquote(name), unquote(subevent_name)}
  #     end
  #   ]
  # end

  defp generator_chunk(param, state) do
    %{gen_body: gen_body(param, state), gen_clauses: gen_clauses(param, state)}
  end

  defp gen_clauses(param, state)

  defp gen_clauses(%{value: _}, _), do: []

  defp gen_clauses(%{type: :arrayed_data}, state) do
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- StreamData.binary()
      end
    ]
  end

  defp gen_clauses(%{type: :command_return}, state) do
    opcode = relative_parameter(-1, state)
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <-
          Harald.Generators.HCI.generate({:return, Harald.HCI.command_name(unquote(opcode))})
      end
    ]
  end

  defp gen_clauses(%{values: %Range{} = values}, state) do
    name = state.ast_map.parameter_var

    [
      quote do
        unquote(name) <- StreamData.member_of(unquote(Macro.escape(values)))
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

  defp look_behind(depth, state) do
    [[deserializer]] = state.ast_map.deserializers
    {:def, _, [ret, _]} = deserializer
    {:deserialize, _, [{:<<>>, _, ret}]} = ret
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

  # defp gen_body(%{type: {:branch, type_id}} = param, state) do
  #   gen_body(Map.put(param, :type, type_id), state)
  # end

  defp gen_body(%{value: value}, _) do
    [Macro.escape(value)]
  end

  defp gen_body(%{type: type}, state)
       when type in [:arrayed_data, :command_return, :null_terminated, :opcode] do
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

  defp ast_map(type, name, prefix)

  defp ast_map(:empty, _, _) do
    %{
      deserializers: [],
      generators: [],
      serializers: []
    }
  end

  defp ast_map(type, name, prefix) do
    %{
      deserializers: ast_map_deserializers(type, name, prefix),
      generators: ast_map_generators(type, name, prefix),
      subevent_name: ast_subevent_name(type, name),
      parameter_var: Macro.var(:v1, __MODULE__),
      parameter_index: 1,
      serializers: ast_map_serializers(type, name, prefix)
    }
  end

  defp ast_subevent_name(:subevent, {_, subevent_name}), do: subevent_name

  defp ast_subevent_name(_, _), do: nil

  defp ast_map_deserializers(:return, name, prefix) do
    parameter = {{:return, name}, {:<<>>, [], prefix}}

    [
      quote do
        def deserialize(unquote(parameter)) do
          %{type: :return, opcode: unquote(name)}
        end
      end
    ]
  end

  defp ast_map_deserializers(:command, name, prefix) do
    [
      quote do
        def deserialize(unquote({:<<>>, [], prefix ++ [0]})) do
          %{type: :command, opcode: unquote(name)}
        end
      end
    ]
  end

  defp ast_map_deserializers(:event, name, prefix) do
    parameter = {:<<>>, [], prefix ++ [Macro.var(:_parameter_total_length, __MODULE__)]}

    [
      quote do
        def deserialize(unquote(parameter)) do
          %{type: :event, event_code: unquote(name)}
        end
      end
    ]
  end

  defp ast_map_deserializers(:subevent, {event_name, _}, prefix) do
    parameter = {:<<>>, [], prefix ++ [Macro.var(:_parameter_total_length, __MODULE__)]}

    [
      quote do
        def deserialize(unquote(parameter)) do
          %{type: :event, event_code: unquote(event_name)}
        end
      end
    ]
  end

  # defp ast_map_deserializers(:subevent, {event_name, subevent_name}, prefix) do
  #   parameter = {:<<>>, [], prefix ++ [Macro.var(:_parameter_total_length, __MODULE__)]}

  #   [
  #     quote do
  #       def deserialize(unquote(parameter)) do
  #         %{type: :event, event_code: unquote(event_name), subevent_code: unquote(subevent_name)}
  #       end
  #     end
  #   ]
  # end

  defp ast_map_generators(:command, name, prefix) do
    [
      quote do
        def generate(unquote(name)) do
          gen all(bin <- StreamData.constant(unquote({:<<>>, [], prefix ++ [0]}))) do
            <<bin::binary>>
          end
        end
      end
    ]
  end

  defp ast_map_generators(:event, name, prefix) do
    [
      quote do
        def generate(unquote(name)) do
          gen all(
                bin <- StreamData.constant(unquote({:<<>>, [], prefix})),
                parameters = <<>>,
                parameter_total_length = byte_size(parameters)
              ) do
            <<bin::binary, parameter_total_length, parameters::binary>>
          end
        end
      end
    ]
  end

  defp ast_map_generators(:subevent, {event_name, subevent_name} = name, prefix) do
    [
      quote do
        def generate(unquote(name)) do
          gen all(
                bin <- StreamData.constant(unquote({:<<>>, [], prefix})),
                parameters = <<>>,
                parameter_total_length = byte_size(parameters)
              ) do
            <<bin::binary, parameter_total_length, parameters::binary>>
          end
        end
      end
    ]
  end

  defp ast_map_generators(:return, name, prefix) do
    [
      quote do
        def generate({:return, unquote(name)}) do
          gen all(bin <- StreamData.constant(unquote({:<<>>, [], prefix}))) do
            <<bin::binary>>
          end
        end
      end
    ]
  end

  defp ast_map_serializers(:command, name, prefix) do
    [
      quote do
        def serialize(%{opcode: unquote(name), type: :command}) do
          unquote({:<<>>, [], prefix ++ [0]})
        end
      end
    ]
  end

  defp ast_map_serializers(:return, name, _) do
    [
      quote do
        def serialize(%{opcode: unquote(name), type: :return}) do
          <<>>
        end
      end
    ]
  end

  defp ast_map_serializers(:event, name, prefix) do
    return =
      {:<<>>, [],
       prefix ++
         [
           Macro.var(:parameter_total_length, __MODULE__),
           {:"::", [], [Macro.var(:parameters, __MODULE__), Macro.var(:binary, __MODULE__)]}
         ]}

    [
      quote do
        def serialize(%{event_code: unquote(name), type: :event}) do
          parameters = <<>>
          parameter_total_length = byte_size(parameters)
          unquote(return)
        end
      end
    ]
  end

  defp ast_map_serializers(:subevent, {event_name, subevent_name}, prefix) do
    return =
      {:<<>>, [],
       prefix ++
         [
           Macro.var(:parameter_total_length, __MODULE__),
           {:"::", [], [Macro.var(:parameters, __MODULE__), Macro.var(:binary, __MODULE__)]}
         ]}

    [
      quote do
        def serialize(%{
              event_code: unquote(event_name),
              type: :event
            }) do
          parameters = <<>>
          parameter_total_length = byte_size(parameters)
          unquote(return)
        end
      end
    ]
  end

  # defp ast_map_serializers(:subevent, {event_name, subevent_name}, prefix) do
  #   return =
  #     {:<<>>, [],
  #      prefix ++
  #        [
  #          Macro.var(:parameter_total_length, __MODULE__),
  #          {:"::", [], [Macro.var(:parameters, __MODULE__), Macro.var(:binary, __MODULE__)]}
  #        ]}

  #   [
  #     quote do
  #       def serialize(%{
  #             event_code: unquote(event_name),
  #             subevent_code: unquote(subevent_name),
  #             type: :event
  #           }) do
  #         parameters = <<>>
  #         parameter_total_length = byte_size(parameters)
  #         unquote(return)
  #       end
  #     end
  #   ]
  # end

  defp spec_unit(name, parameters), do: %{name: name, parameters: parameters}
end
