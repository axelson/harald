defmodule Harald.HCITest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Harald.HCI

  doctest Harald.HCI, import: true

  describe "opcode/1" do
    property "symmetric (de)serialization" do
      check all(
              {ogf, ocf} = opcode_tuple <- StreamData.tuple({integer(0..63), integer(0..1023)}),
              <<opcode_integer::little-size(16)>> = <<ogf::size(6), ocf::size(10)>>,
              opcode = <<opcode_integer::size(16)>>
            ) do
        assert opcode == HCI.opcode(opcode_tuple)
        assert opcode_tuple == HCI.opcode(opcode)
      end
    end
  end
end
