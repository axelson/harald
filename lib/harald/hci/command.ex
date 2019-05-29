defmodule Harald.HCI.Command do
  @moduledoc """
  Serialization module for HCI Commands.

  > The HCI Command Packet is used to send commands to the Controller from the Host.

  Reference: Version 5.0, Vol 2, Part E, 5.4.1
  """

  alias Harald.HCI.Command.LEController

  @definitions %{0x01 => LEController}

  @callback deserialize_return(pos_integer(), binary()) :: {:ok, any()} | :error

  @doc """
  Returns a map relating opcodes to implementation modules.

  The return is structured like: `%{ogf => %{ocf => {binary_opcode, implementation_module}}}`.
  """
  def definitions, do: @definitions

  @doc """
  Returns the module implementing `ogf` functions.
  """
  def group_from_ogf(ogf)

  Enum.each(@definitions, fn {ogf, module} ->
    def group_from_ogf(unquote(ogf)), do: {:ok, unquote(module)}
  end)

  def group_from_ogf(_), do: :error
end
