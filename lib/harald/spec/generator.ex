defmodule Harald.Spec.Generator do
  use ExUnitProperties
  @doc false
  def define_generators(spec) do
    %{ast: core_ast, commands: commands, events: events} = core_generators(spec.gen_maps)
    %{ast: extra_ast} = extra_generators(commands, events)

    core_ast ++ extra_ast
  end

  @doc false
  def define_serializers(spec) do
    Enum.reduce(spec.gen_maps, [], fn
      {:events, value}, acc ->
        Enum.reduce(value, acc, fn
          %{
            bin_body: bin_body,
            bin_pattern: bin_pattern,
            parameters: parameters
          },
          acc ->
            ast =
              quote location: :keep, generated: true do
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

      _, acc ->
        acc
    end)
  end

  defp core_generators(spec) do
    Enum.reduce(spec, %{ast: [], commands: [], events: [], packets: []}, fn
      {section, sub_spec}, acc -> generator_ast(section, sub_spec, acc)
    end)
  end

  # defp acl_data_generators do
  #   quote do
  #     def generate(:acl_data) do
  #       gen all(
  #             command <- StreamData.member_of(unquote(commands)),
  #             bin <- generate(command)
  #           ) do
  #         <<handle::size(12), flags::size(4), length::size(16)>> <> data
  #       end
  #     end
  #   end
  # end

  defp extra_generators(commands, events) do
    %{
      ast: [
        quote do
          def generate(:packet) do
            gen all(
                  packet_type <- member_of([:event, :acl_data]),
                  bin <- generate(packet_type)
                ) do
              bin
            end
          end

          # def generate(:acl_data) do
          #   gen all(
          #         command <- StreamData.member_of(unquote(commands)),
          #         bin <- generate(command)
          #       ) do
          #     <<2, 0::size(16), 1::little-size(16), 69>>
          #     <<2, handle::size(12), flags::size(4), length::size(16)>> <> data
          #   end
          # end

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
    }
  end

  defp generator_ast(:spec, _, acc), do: acc

  defp generator_ast(label, sub_spec, acc) do
    Enum.reduce(sub_spec, acc, fn
      %{gen_body: gen_body, gen_clauses: gen_clauses, name: name}, acc ->
        ast =
          quote do
            def generate(unquote(name)) do
              gen all(unquote_splicing(gen_clauses)) do
                unquote(gen_body)
              end
            end
          end

        IO.inspect(Macro.to_string(ast), pretty: true)

        %{
          acc
          | :ast => acc.ast ++ [ast],
            label => [name | acc[label]]
        }
    end)
  end
end
