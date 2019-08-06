defmodule Harald.HCI.LEControllerTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Harald.HCI.Command.LEController

  doctest LEController, import: true

  describe "command/3" do
    test "LE_Set_Scan_Enable" do
      check all(
              enable <- StreamData.boolean(),
              filter_duplicates <- StreamData.boolean()
            ) do
        e_num = if enable, do: 1, else: 0
        f_num = if filter_duplicates, do: 1, else: 0

        assert <<1, 12, 32, 2, e_num, f_num>> ==
                 LEController.command(:hci_le_set_scan_enable, %{
                   le_scan_enable: enable,
                   filter_duplicates: filter_duplicates
                 })
      end
    end
  end
end
