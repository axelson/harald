defmodule Harald.HCI.Command.LEController do
  @moduledoc """
  > The LE Controller Commands provide access and control to various capabilities of the Bluetooth
  > hardware, as well as methods for the Host to affect how the Link Layer manages the piconet,
  > and controls connections.

  Reference: Version 5.0, Vol 2, Part E, 7.8
  """

  alias Harald.{ErrorCode, HCI, HCI.Command}

  @behaviour Command

  def command(ocf, parameters), do: HCI.command(0x08, ocf, parameters)

  @impl Command
  def deserialize_return(ocf, bin)

  def deserialize_return(ocf, <<status>>)
      when ocf in [
             0x000B,
             0x000C
           ] do
    {:ok, ErrorCode.name(status)}
  end

  def deserialize_return(_, _), do: :error
end
