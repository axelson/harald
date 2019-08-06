defmodule Harald do
  @moduledoc """
  An Elixir Bluetooth Host library.

  High level Bluetooth functionality.
  """

  alias Harald.HCI

  defdelegate deserialize(data), to: HCI

  defdelegate serialize(event_code, parameters), to: HCI

  defdelegate serialize(ogf, ocf, parameters), to: HCI
end
