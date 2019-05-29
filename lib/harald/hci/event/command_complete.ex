defmodule Harald.HCI.Event.CommandComplete do
  @moduledoc """
  > The Command Complete event is used by the Controller for most commands to transmit return
  > status of a command and the other event parameters that are specified for the issued HCI
  > command.

  Reference: Version 5.0, Vol 2, Part E, 7.7.14
  """

  alias Harald.{HCI, HCI.Command, Serializable}

  @behaviour Serializable

  @type t :: %__MODULE__{}

  defstruct [:packets, :opcode, :data]

  @event_code 0x0E

  @doc """
  Returns the Command Complete event code.
  """
  def event_code, do: @event_code

  @impl Serializable
  def serialize(%__MODULE__{packets: packets, opcode: {ogf, ocf}, data: data}) do
    opcode = HCI.opcode(ogf, ocf)
    {:ok, <<packets, opcode::binary, data::binary>>}
  end

  @impl Serializable
  def deserialize(<<packets, opcode::integer-little-size(16), tail::binary>>) do
    <<ogf::size(6), ocf::size(10)>> = <<opcode::size(16)>>

    {status, data} =
      case deserialize_data({ogf, ocf}, tail) do
        {:ok, data} -> {:ok, data}
        :error -> {:error, tail}
      end

    {status, %__MODULE__{packets: packets, opcode: {ogf, ocf}, data: data}}
  end

  def deserialize(bin), do: {:error, bin}

  defp deserialize_data({ogf, ocf}, bin) do
    case Command.group_from_ogf(ogf) do
      {:ok, module} -> module.deserialize_return(ocf, bin)
      :error -> :error
    end
  end
end
