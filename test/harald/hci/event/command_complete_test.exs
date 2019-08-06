defmodule Harald.HCI.Event.CommandCompleteTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Harald.Generators.HCI.Event.CommandComplete, as: CommandCompleteGen
  alias Harald.HCI.Event.CommandComplete
  require Harald.Serializable, as: Serializable

  doctest CommandComplete, import: true

  property "symmetric (de)serialization" do
    check all(
            bin <- CommandCompleteGen.parameters(),
            rand_bin <- StreamData.binary()
          ) do
      Serializable.assert_symmetry(CommandComplete, bin)
      Serializable.assert_symmetry(CommandComplete, rand_bin)
    end
  end
end
