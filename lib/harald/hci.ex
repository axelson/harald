defmodule Harald.HCI do
  @moduledoc """
  > The HCI provides a uniform interface method of accessing a Bluetooth Controller’s
  > capabilities.

  Reference: Version 5.0, Vol. 2, Part E, 1
  """

  alias Harald.{HCI.Event, Serializable}
  require Harald.Spec, as: Spec

  @behaviour Serializable

  @type command :: binary()
  @type event :: binary()

  @typedoc """
  OpCode Group Field.

  See `t:opcode/0`
  """
  @type ogf :: non_neg_integer()

  @typedoc """
  OpCode Command Field.

  See `t:opcode/0`
  """
  @type ocf :: non_neg_integer()

  @typedoc """
  > Each command is assigned a 2 byte Opcode used to uniquely identify different types of
  > commands. The Opcode parameter is divided into two fields, called the OpCode Group Field (OGF)
  > and OpCode Command Field (OCF). The OGF occupies the upper 6 bits of the Opcode, while the OCF
  > occupies the remaining 10 bits. The OGF of 0x3F is reserved for vendor-specific debug
  > commands. The organization of the opcodes allows additional information to be inferred without
  > fully decoding the entire Opcode.

  Reference: Version 5.0, Vol. 2, Part E, 5.4.1
  """
  @type opcode :: binary()

  @type opt :: boolean() | binary()
  @type opts :: binary() | [opt()]

  @spec opcode(ogf(), ocf()) :: opcode()
  def opcode(ogf, ocf) do
    <<opcode::size(16)>> = <<ogf::size(6), ocf::size(10)>>
    <<opcode::little-size(16)>>
  end

  def opcode(<<opcode::little-size(16)>>) do
    case <<opcode::size(16)>> do
      <<ogf::size(6), ocf::size(10)>> -> {:ok, {ogf, ocf}}
      _ -> :error
    end
  end

  Spec.define_serializers()
end
