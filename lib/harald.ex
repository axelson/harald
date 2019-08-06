defmodule Harald do
  @moduledoc """
  An incomplete and opinionated abstraction over Bluetooth functionality.

  Bluetooth is exceptionally configurable and there are many ways to achieve similar outcomes. As
  such, leverage this module if convenient, otherwise interact with a `Harald.Transport` directly
  to send commands and react to events exactly as required to achieve the desired outcome.
  """

  alias Harald.{HCI, Server}

  @typedoc """
  A request is considered eroneous in these cases:

  - `:setup_incomplete` - The underlying `Harald.Transport` has not yet completed setting up the
    Bluetooth Controller.
  - `:already_scanning` - There is already a scan in progress.
  - `:command_complete_timeout` - A Command Complete event is not received in a timely fashion.
  - A Command Complete event itself is the error reason when it includes a status parameter that
    could indicate success, but something other than success is indicated.
  """
  @type error_reason :: :timeout | :command_complete_timeout | %{event: :hci_command_complete}

  @typedoc """
  Tuple describing why a request was eroneous.
  """
  @type error :: {:error, error_reason()}

  @doc """
  Child specification that delegates to `Harald.Server`.
  """
  def child_spec(%{} = args) do
    %{id: args.namespace, start: {Server, :start_link, args}}
  end

  @typedoc """
  The options accepted by `scan/2`.
  """
  @type scan_opts() :: [{:duration, non_neg_integer()}]

  @doc """
  Performs a scan for nearby devices.

  Though the time before this function returns is mostly dictated by the `:duration` option, it
  will not return until a Command Complete event is received for the command sent to stop
  scanning.

  ## Options

  - `:duration` - The time in milliseconds after starting the scan that the scan should be
    stopped.
  """
  @spec scan(Server.namespace(), scan_opts()) :: {:ok, []} | error()
  def scan(namespace, opts \\ []) do
    args =
      opts
      |> Enum.into(%{duration: 5_000})
      |> Map.take([:duration])

    timeout = args.duration + 5_000
    Server.call(namespace, {:scan, args}, timeout)
  end

  defdelegate deserialize(binary), to: HCI

  defdelegate serialize(map), to: HCI
end
