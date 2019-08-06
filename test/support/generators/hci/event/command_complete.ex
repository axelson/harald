defmodule Harald.Generators.HCI.Event.CommandComplete do
  @moduledoc """
  StreamData generators for Command Complete event.

  Reference: Version 5.0, Vol 2, Part E, 7.7.14
  """

  use ExUnitProperties

  def parameters do
    gen all(
          packets <- StreamData.binary(length: 1),
          opcode <- StreamData.binary(length: 2),
          data <- StreamData.binary()
        ) do
      <<packets::binary, opcode::binary, data::binary>>
    end
  end
end
