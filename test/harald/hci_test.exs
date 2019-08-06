defmodule Harald.HCITest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Harald.Generators.HCI, as: HCIGen
  alias Harald.HCI

  doctest Harald.HCI, import: true

  test "opcode/2" do
    check all(
            ogf <- StreamData.integer(0..63),
            ocf <- StreamData.integer(0..1023)
          ) do
      <<opcode::size(16)-little>> = HCI.opcode(ogf, ocf)
      assert <<^ogf::size(6), ^ocf::size(10)>> = <<opcode::size(16)>>
    end
  end

  property "symmetric (de)serialization" do
    check(
      all(bin <- HCIGen.generate()) do
        case HCI.deserialize(bin) do
          {:ok, data} ->
            assert {:ok, bin2} = HCI.serialize(data)
            assert :binary.bin_to_list(bin) == :binary.bin_to_list(bin2)
        end
      end
    )
  end
end
