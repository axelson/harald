[
  error_codes: %{
    0x00 => "Success",
    0x01 => "Unknown HCI Command",
    0x02 => "Unknown Connection Identifier",
    0x03 => "Hardware Failure",
    0x04 => "Page Timeout",
    0x05 => "Authentication Failure",
    0x06 => "PIN or Key Missing",
    0x07 => "Memory Capacity Exceeded",
    0x08 => "Connection Timeout",
    0x09 => "Connection Limit Exceeded",
    0x0A => "Synchronous Connection Limit To A Device Exceeded",
    0x0B => "Connection Already Exists",
    0x0C => "Command Disallowed",
    0x0D => "Connection Rejected due to Limited Resources",
    0x0E => "Connection Rejected Due To Security Reasons",
    0x0F => "Connection Rejected due to Unacceptable BD_ADDR",
    0x10 => "Connection Accept Timeout Exceeded",
    0x11 => "Unsupported Feature or Parameter Value",
    0x12 => "Invalid HCI Command Parameters",
    0x13 => "Remote User Terminated Connection",
    0x14 => "Remote Device Terminated Connection due to Low Resources",
    0x15 => "Remote Device Terminated Connection due to Power Off",
    0x16 => "Connection Terminated By Local Host",
    0x17 => "Repeated Attempts",
    0x18 => "Pairing Not Allowed",
    0x19 => "Unknown LMP PDU",
    0x1A => "Unsupported Remote Feature / Unsupported LMP Feature",
    0x1B => "SCO Offset Rejected",
    0x1C => "SCO Interval Rejected",
    0x1D => "SCO Air Mode Rejected",
    0x1E => "Invalid LMP Parameters / Invalid LL Parameters",
    0x1F => "Unspecified Error",
    0x20 => "Unsupported LMP Parameter Value / Unsupported LL Parameter Value",
    0x21 => "Role Change Not Allowed",
    0x22 => "LMP Response Timeout / LL Response Timeout",
    0x23 => "LMP Error Transaction Collision / LL Procedure Collision",
    0x24 => "LMP PDU Not Allowed",
    0x25 => "Encryption Mode Not Acceptable",
    0x26 => "Link Key cannot be Changed",
    0x27 => "Requested QoS Not Supported",
    0x28 => "Instant Passed",
    0x29 => "Pairing With Unit Key Not Supported",
    0x2A => "Different Transaction Collision",
    0x2B => "Reserved for Future Use (0x2B)",
    0x2C => "QoS Unacceptable Parameter",
    0x2D => "QoS Rejected",
    0x2E => "Channel Classification Not Supported",
    0x2F => "Insufficient Security",
    0x30 => "Parameter Out Of Mandatory Range",
    0x31 => "Reserved for Future Use (0x31)",
    0x32 => "Role Switch Pending",
    0x33 => "Reserved for Future Use (0x33)",
    0x34 => "Reserved Slot Violation",
    0x35 => "Role Switch Failed",
    0x36 => "Extended Inquiry Response Too Large",
    0x37 => "Secure Simple Pairing Not Supported By Host",
    0x38 => "Host Busy - Pairing",
    0x39 => "Connection Rejected due to No Suitable Channel Found",
    0x3A => "Controller Busy",
    0x3B => "Unacceptable Connection Parameters",
    0x3C => "Advertising Timeout",
    0x3D => "Connection Terminated due to MIC Failure",
    0x3E => "Connection Failed to be Established",
    0x3F => "MAC Connection Failed",
    0x40 => "Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock Dragging",
    0x41 => "Type0 Submap Not Defined",
    0x42 => "Unknown Advertising Identifier",
    0x43 => "Limit Reached",
    0x44 => "Operation Cancelled by Host"
  },
  packets: [
    # %{
    #   name: "Command",
    #   id: 1,
    #   parameters: [
    #     %{name: "Op_Code", type: "opcode"},
    #     %{name: "Parameter_Total_Length", size: 8, values: 0..252},
    #     %{name: "Parameters", size: "Parameter_Total_Length"}
    #   ]
    # },
    # %{
    #   name: "ACL Data",
    #   id: 2,
    #   parameters: [
    #     %{name: "Handle", type: "handle"},
    #     %{name: "Packet_Boundary_Flag", type: "flag"},
    #     %{name: "Broadcast_Flag", size: 2, values: 0..1},
    #     %{name: "Data_Total_Length", size: 8 * 2},
    #     %{name: "Data", size: "Data_Total_Length"}
    #   ]
    # },
    # %{
    #   name: "Synchronous Data",
    #   id: 3,
    #   parameters: [
    #     %{name: "Connection_Handle", type: "handle"},
    #     %{name: "Packet_Status_Flag", type: "flag"},
    #     %{size: 2, type: "rfu"},
    #     %{name: "Data_Total_Length"},
    #     %{name: "Data", size: "Data_Total_Length"}
    #   ]
    # },
    # %{
    #   name: "Event",
    #   id: 4,
    #   parameters: [
    #     %{name: "Event_Code"},
    #     %{name: "Parameter_Total_Length"},
    #     %{name: "Parameters", size: "Parameter_Total_Length"}
    #   ]
    # }
  ],
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
    # %{
    #   id: 8,
    #   commands: [
    #     %{
    #       name: "HCI_LE_Set_Scan_Enable",
    #       id: 12,
    #       parameters: [
    #         %{name: "LE_Scan_Enable", size: 8, type: :boolean},
    #         %{name: "Filter_Duplicates", size: 8, type: :boolean}
    #       ],
    #       return: [%{name: "Status", type: :error_code}]
    #     }
    #   ]
    # }
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
    # %{
    #   name: "HCI_LE_Meta",
    #   id: 62,
    #   subevents: [
    #     %{
    #       event: "HCI_LE_Advertising_Report",
    #       parameters: [
    #         %{name: "Subevent_Code", value: 2},
    #         %{name: "Num_Reports", size: 8, type: :integer},
    #         %{
    #           multiplier: "Num_Reports",
    #           parameters: [
    #             %{name: "Event_Type", size: 8, type: :advertising_pdu},
    #             %{name: "Address_Type", size: 8, type: :integer},
    #             %{name: "Address", size: 8 * 6, type: :integer},
    #             %{name: "Length_Data", size: 8, type: :integer},
    #             %{name: "Data", size: "Length_Data", type: :binary},
    #             %{name: "RSS", size: 8, type: :integer}
    #           ],
    #           type: :arrayed_data
    #         }
    #       ]
    #     }
    #   ]
    # }
  ]
]
