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
              %{name: "Command_Opcode", type: :opcode},
              %{name: "Return_Parameter(s)", type: :command_return}
            ]
          },
          %{
            name: "HCI_LE_Meta",
            id: 62,
            subevents: [
              %{
                name: "HCI_LE_Advertising_Report",
                parameters: [
                  %{name: "Subevent_Code", type: :subevent_code, value: 2},
                  %{name: "Num_Reports", size: 8, type: :integer, values: 1..25},
                  %{
                    multiplier: "Num_Reports",
                    name: :reports,
                    parameters: [
                      %{name: "Event_Type", size: 8, type: :advertising_pdu},
                      %{name: "Address_Type", size: 8, type: :integer},
                      %{name: "Address", size: 8 * 6, type: :integer},
                      %{name: "Length_Data", size: 8, type: :integer},
                      %{name: "Data", size: "Length_Data", type: :binary},
                      %{name: "RSS", size: 8, type: :integer}
                    ],
                    type: :arrayed_data
                  }
                ]
              }
            ]
          }
        ]
      ]

      # assert general response

      assert %{ast_maps: actual_ast_maps, types: types} = Parser.parse(spec)
      assert 4 = length(actual_ast_maps)

      # assert subevent

      expected_deserializers = [
        quote context: Parser do
          def deserialize(<<4, 62, _parameter_total_length, 2, v2::size(8), v3::binary>>) do
            %{
              :type => :event,
              :event_code => "HCI_LE_Meta",
              "Subevent_Code" => "HCI_LE_Advertising_Report",
              "Num_Reports" => v2,
              :reports => v3
            }
          end
        end
      ]

      opcodes = types.opcode.values

      expected_generators = [
        quote context: Parser do
          def generate({"HCI_LE_Meta", "HCI_LE_Advertising_Report"}) do
            gen all(
                  bin <- StreamData.constant(<<4, 62>>),
                  v2 <- StreamData.member_of(unquote(Macro.escape(1..25))),
                  v3 <- StreamData.binary(),
                  parameters = <<2, v2, v3::binary>>,
                  parameter_total_length = byte_size(parameters)
                ) do
              <<bin::binary, parameter_total_length, parameters::binary>>
            end
          end
        end
      ]

      expected_serializers = [
        quote context: Parser do
          def serialize(%{
                :event_code => "HCI_LE_Meta",
                :type => :event,
                "Subevent_Code" => "HCI_LE_Advertising_Report",
                "Num_Reports" => v2,
                :reports => v3
              }) do
            parameters = <<2, v2::size(8), v3::binary>>
            parameter_total_length = byte_size(parameters)
            <<4, 62, parameter_total_length, parameters::binary>>
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

      # assert event

      expected_deserializers = [
        quote context: Parser do
          def deserialize(
                <<4, 14, _parameter_total_length, v1::size(8), v2::binary-size(2), v3::binary>>
              ) do
            v2 = Harald.HCI.command_name(v2)
            v3 = Harald.HCI.deserialize({{:return, v2}, v3})

            %{
              :type => :event,
              :event_code => "HCI_Command_Complete",
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
                  bin <- StreamData.constant(<<4, 14>>),
                  v1 <- StreamData.integer(0..255),
                  v2 <- StreamData.member_of(unquote(opcodes)),
                  v3 <- Harald.Generators.HCI.generate({:return, Harald.HCI.command_name(v2)}),
                  parameters = <<v1, v2::binary, v3::binary>>,
                  parameter_total_length = byte_size(parameters)
                ) do
              <<bin::binary, parameter_total_length, parameters::binary>>
            end
          end
        end
      ]

      expected_serializers = [
        quote context: Parser do
          def serialize(%{
                :event_code => "HCI_Command_Complete",
                :type => :event,
                "Num_HCI_Command_Packets" => v1,
                "Command_Opcode" => v2,
                "Return_Parameter(s)" => v3
              }) do
            v2 = Harald.HCI.command_opcode(v2)
            v3 = Harald.HCI.serialize(v3)
            parameters = <<v1::size(8), v2::binary-size(2), v3::binary>>
            parameter_total_length = byte_size(parameters)
            <<4, 14, parameter_total_length, parameters::binary>>
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

      # assert command

      expected_deserializers = [
        quote context: Parser do
          def deserialize(<<1, 20, 12, 0>>) do
            %{type: :command, opcode: "HCI_Read_Local_Name"}
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
          def serialize(%{opcode: "HCI_Read_Local_Name", type: :command}) do
            <<1, 20, 12, 0>>
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

      # assert command return

      expected_deserializers = [
        quote context: Parser do
          def deserialize({{:return, "HCI_Read_Local_Name"}, <<v1, v2::binary-size(248)>>}) do
            v1 = Harald.HCI.error_desc(v1)
            [head | tail] = String.split(v2, <<0>>)
            v2 = {head, Enum.join(tail)}

            %{
              :type => :return,
              :opcode => "HCI_Read_Local_Name",
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
                :opcode => "HCI_Read_Local_Name",
                :type => :return,
                "Status" => v1,
                "Local_Name" => v2
              }) do
            v1 = Harald.HCI.error_code(v1)
            v2 = elem(v2, 0) <> <<0>> <> elem(v2, 1)
            <<v1, v2::binary-size(248)>>
          end
        end
      ]

      assert %{
               deserializers: actual_deserializers,
               generators: actual_generators,
               serializers: actual_serializers
             } = Enum.at(actual_ast_maps, 3)

      assert expected_deserializers == actual_deserializers
      assert expected_generators == actual_generators
      assert expected_serializers == actual_serializers
    end
  end
end
