defmodule Harald.HCI.Command.LEController do
  @moduledoc """
  > The LE Controller Commands provide access and control to various capabilities of the Bluetooth
  > hardware, as well as methods for the Host to affect how the Link Layer manages the piconet,
  > and controls connections.

  Reference: Version 5.0, Vol 2, Part E, 7.8
  """

  alias Harald.{ErrorCode, HCI, HCI.Command}

  @behaviour Command

  @ogf 0x08

  @doc """
  Generate the binary command identified by `ocf`.

  ## LE_Set_Scan_Enable

  Reference: Version 5.0, Vol 2, Part E, 7.8.11

      iex> command(0x000C, true)
      <<12, 32, 2, 1, 0>>

      iex> command(0x000C, false)
      <<12, 32, 2, 0, 0>>

  ## LE_Set_Scan_Parameters

  Reference: Version 5.0, Vol 2, Part E, 7.8.10

      iex> command(0x000B, le_scan_type: 0x01)
      <<11, 32, 7, 1, 16, 0, 16, 0, 0, 0>>

      iex> command(
      iex>   0x000B,
      iex>   le_scan_type: 0x01,
      iex>   le_scan_interval: 0x0004,
      iex>   le_scan_window: 0x0004,
      iex>   own_address_type: 0x01,
      iex>   scanning_filter_policy: 0x01
      iex> )
      <<11, 32, 7, 1, 4, 0, 4, 0, 1, 1>>
  """
  def command(ocf, parameters)

  # LE_Set_Scan_Parameters
  def command(0x000B, parameters) do
    # Defaults according to the Bluetooth Core Spec v5.
    params =
      [
        le_scan_type: 0x00,
        le_scan_interval: 0x0010,
        le_scan_window: 0x0010,
        own_address_type: 0x00,
        scanning_filter_policy: 0x00
      ]
      |> Keyword.merge(parameters)

    opts = <<
      params[:le_scan_type],
      params[:le_scan_interval]::size(16)-little,
      params[:le_scan_window]::size(16)-little,
      params[:own_address_type],
      params[:scanning_filter_policy]
    >>

    @ogf
    |> HCI.opcode(0x000B)
    |> HCI.command(opts)
  end

  # LE_Set_Scan_Enable
  def command(0x000C, enable, filter_duplicates \\ false) do
    @ogf
    |> HCI.opcode(0x000C)
    |> HCI.command([enable, filter_duplicates])
  end

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
