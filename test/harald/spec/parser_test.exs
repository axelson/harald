defmodule Harald.Spec.ParserTest do
  use ExUnit.Case, async: true
  alias Harald.Spec.Parser

  doctest Harald.HCI, import: true

  describe "command_groups" do
    test "command" do
      spec = [
        error_codes: %{
          0x00 => "Success",
          0x01 => "Unknown HCI Command"
        },
        packets: [],
        command_groups: [
          %{
            id: 3,
            commands: [
              %{
                name: "HCI_Read_Local_Name",
                id: 20,
                parameters: [],
                return: [
                  %{name: "Status", type: :error_code},
                  %{name: "Local_Name", size: 8 * 248, type: :null_terminated}
                ]
              }
            ]
          }
        ],
        events: [
          %{
            name: "HCI_Command_Complete",
            id: 14,
            parameters: [
              %{name: "Num_HCI_Command_Packets"},
              %{name: "Command_Opcode", type: {:branch, :opcode}},
              %{name: "Return_Parameter(s)", type: :command_return}
            ]
          }
        ]
      ]

      # assert general response

      assert %{ast_maps: actual_ast_maps, types: types} = Parser.parse(spec)
      assert 3 = length(actual_ast_maps)

      # assert event

      expected_deserializers = [
        quote context: Parser do
          def deserialize(<<4, _size, v1::size(8), v2::size(16), v3::binary>>) do
            v3 = Harald.HCI.deserialize(v3)

            %{
              :type => :event,
              :name => "HCI_Command_Complete",
              "Num_HCI_Command_Packets" => v1,
              "Command_Opcode" => v2,
              "Return_Parameter(s)" => v3
            }
          end
        end
      ]

      opcodes = types.opcode.values

      expected_generators = [
        quote context: Parser do
          def generate("HCI_Command_Complete") do
            gen all(
                  bin <- StreamData.constant(<<4>>),
                  v1 <- StreamData.integer(0..255),
                  v2 <- StreamData.member_of(unquote(opcodes)),
                  v3 <- Harald.Generators.HCI.generate({:return, "HCI_Read_Local_Name"})
                ) do
              <<bin::binary, v1, v2::binary, v3::binary>>
            end
          end
        end
      ]

      expected_serializers = [
        quote context: Parser do
          def serialize(%{
                :type => :event,
                :name => "HCI_Command_Complete",
                "Num_HCI_Command_Packets" => v1,
                "Command_Opcode" => v2,
                "Return_Parameter(s)" => v3
              }) do
            parameters = <<v1::size(8), v2::size(16), v3::binary>>
            parameter_total_length = byte_size(parameters)
            <<4, parameter_total_length, parameters::binary>>
          end
        end
      ]

      assert %{
               deserializers: actual_deserializers,
               generators: actual_generators,
               serializers: actual_serializers
             } = Enum.at(actual_ast_maps, 0)

      assert expected_deserializers == actual_deserializers
      assert expected_generators == actual_generators
      assert expected_serializers == actual_serializers

      # assert command

      expected_deserializers = [
        quote context: Parser do
          def deserialize(<<1, 20, 12, 0>>) do
            %{type: :command, name: "HCI_Read_Local_Name"}
          end
        end
      ]

      expected_generators = [
        quote context: Parser do
          def generate("HCI_Read_Local_Name") do
            gen all(bin <- StreamData.constant(<<1, 20, 12, 0>>)) do
              <<bin::binary>>
            end
          end
        end
      ]

      expected_serializers = [
        quote context: Parser do
          def serialize(%{type: :command, name: "HCI_Read_Local_Name"}) do
            <<1, 20, 12, 0>>
          end
        end
      ]

      assert %{
               deserializers: actual_deserializers,
               generators: actual_generators,
               serializers: actual_serializers
             } = Enum.at(actual_ast_maps, 1)

      assert expected_deserializers == actual_deserializers
      assert expected_generators == actual_generators
      assert expected_serializers == actual_serializers

      # assert command return

      expected_deserializers = [
        quote context: Parser do
          def deserialize({{:return, "HCI_Read_Local_Name" = name}, <<v1, v2::binary-size(248)>>}) do
            v1 = Harald.HCI.error_desc(v1)

            %{
              :type => :return,
              :name => name,
              "Status" => v1,
              "Local_Name" => v2
            }
          end
        end
      ]

      expected_generators = [
        quote context: Parser do
          def generate({:return, "HCI_Read_Local_Name"}) do
            gen all(
                  bin <- StreamData.constant(<<>>),
                  v1 <- StreamData.member_of([0, 1]),
                  v2 <- Harald.Generators.HCI.generate(:null_terminated, length: 248)
                ) do
              <<bin::binary, v1, v2::binary>>
            end
          end
        end
      ]

      expected_serializers = [
        quote context: Parser do
          def serialize(%{
                :type => :return,
                :name => "HCI_Read_Local_Name",
                "Status" => v1,
                "Local_Name" => v2
              }) do
            v1 = Harald.HCI.error_code(v1)
            <<v1, v2::binary-size(248)>>
          end
        end
      ]

      assert %{
               deserializers: actual_deserializers,
               generators: actual_generators,
               serializers: actual_serializers
             } = Enum.at(actual_ast_maps, 2)

      assert expected_deserializers == actual_deserializers
      assert expected_generators == actual_generators
      assert expected_serializers == actual_serializers
    end
  end
end
