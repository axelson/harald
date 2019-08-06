%{
  packets: [
    %{
      name: :command,
      indicator: 0x01,
      parameters: [
        %{name: "Op_Code", size: 8 * 2, type: :op_code},
        %{name: "Parameter_Total_Length", size: 8, type: :integer},
        %{name: "Parameters", size: "Parameter_Total_Length", type: :parameters}
      ]
    },
    %{
      name: :event,
      indicator: 0x04,
      parameters: [
        %{name: "Event_Code", size: 8, type: :integer},
        %{name: "Parameter_Total_Length", size: 8, type: :integer},
        %{name: "Parameters", size: "Parameter_Total_Length", type: :parameters}
      ]
    }
  ],
  command_groups: %{
    0x03 => [
      %{
        command: "HCI_Read_Local_Name",
        ocf: 0x0014,
        parameters: [],
        return: [
          %{name: "Status", size: 8, type: :error_code},
          %{name: "Local_Name", size: 8 * 248, type: :null_terminated}
        ]
      }
    ],
    0x08 => [
      %{
        command: "HCI_LE_Set_Scan_Enable",
        ocf: 0x000C,
        parameters: [
          %{name: "LE_Scan_Enable", size: 8, type: :boolean},
          %{name: "Filter_Duplicates", size: 8, type: :boolean}
        ],
        return: [%{name: "Status", size: 8, type: :error_code}]
      }
    ]
  },
  events: [
    %{
      event: "HCI_Command_Complete",
      event_code: 0x0E,
      parameters: [
        %{name: "Num_HCI_Command_Packets", size: 8, type: :integer},
        %{name: "Command_Opcode", size: 8 * 2, type: :op_code},
        %{name: "Return_Parameter(s)", size: 8 * 248, type: :parameters}
      ]
    },
    %{
      event: "HCI_LE_Meta",
      event_code: 0x3E,
      subevents: [
        %{
          event: "HCI_LE_Advertising_Report",
          parameters: [
            %{name: "Subevent_Code", value: 0x02, size: 8, type: :integer},
            %{name: "Num_Reports", size: 8, type: :integer},
            %{
              multiplier: "Num_Reports",
              parameters: [
                %{name: "Event_Type", size: 8, type: :advertising_pdu},
                %{name: "Address_Type", size: 8, type: :integer},
                %{name: "Address", size: 8 * 6, type: :integer},
                %{name: "Length_Data", size: 8, type: :integer},
                %{name: "Data", size: "Length_Data", type: :parameters},
                %{name: "RSS", size: 8, type: :integer}
              ],
              type: :arrayed_data
            }
          ]
        }
      ]
    }
  ]
}
