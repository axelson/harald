defmodule Harald.Server do
  @moduledoc """
  Safely manages the lifecycle of and interaction with a `Harald.Transport`.

  See `Harald.child_spec/1`.

  Users of Harald will typically execute functionality by calling functions on `Harald`. This
  module allows for

  - messages to be received from a `Harald.Transport` process
  - state to be accumulated

  transparently to the users of Harald.
  """

  use GenServer
  alias Harald.{HCI, Transport}
  require Harald.Log, as: Log

  @typedoc """
  An atom identifier to namespace `Harald.Server`s.
  """
  @type namespace :: atom()

  @doc false
  def child_spec(_) do
    raise "Harald.Server should not be referenced directly in a Supervisor's children. See Harald.child_spec/1."
  end

  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: name(args.namespace))
  end

  @impl GenServer
  def init(args) do
    {:ok, pid} = Transport.start_link(args)
    Log.debug("transport started", %{pid: pid})
    {:ok, %{scan_from: nil, reports: %{}, namespace: args.namespace}}
  end

  @doc """
  Makes a synchronous call to the `Harald.Server` identified by `namespace`.
  """
  def call(namespace, request, timeout) do
    namespace
    |> name()
    |> GenServer.call(request, timeout)
  end

  @impl GenServer
  def handle_call({:scan, args}, from, %{from: nil} = state) do
    bin =
      HCI.serialize(%{
        command: :hci_le_set_scan_enable,
        le_scan_enable: true,
        filter_duplicates: false
      })

    :ok = Transport.send_binary(state.ns, bin)
    Process.send_after(self(), :stop_scan, args.duration)
    {:noreply, %{state | from: from}}
  end

  def handle_call({:scan, _}, _, state), do: {:reply, {:error, :already_scanning}, state}

  @impl GenServer
  def handle_info(:stop_scan, state) do
    bin =
      HCI.serialize(%{
        command: :hci_le_set_scan_enable,
        le_scan_enable: false,
        filter_duplicates: false
      })

    :ok = Transport.send_binary(state.ns, bin)
    {:noreply, state}
  end

  def handle_info(
        {:bluetooth_event,
         %{
           event: :hci_le_meta,
           subevent: %{event: :hci_le_advertising_report, reports: new_reports}
         }},
        state
      ) do
    reports =
      Enum.reduce(new_reports, state.reports, fn report, reports ->
        Map.put(reports, report.address, report)
      end)

    {:noreply, %{state | reports: reports}}
  end

  def handle_info(
        {:bluetooth_event,
         %{event: :hci_command_complete, return_parameters: %{status: "Success"}}},
        state
      ) do
    GenServer.reply(state.from, state.devices)
    {:noreply, %{state | devices: %{}, from: nil}}
  end

  def handle_info({:bluetooth_event, unhandled_bluetooth_event}, state) do
    Log.debug("unhandled bluetooth event", %{event: unhandled_bluetooth_event})
    {:noreply, state}
  end

  defp name(namespace), do: String.to_atom("#{__MODULE__}.namespace.#{namespace}")
end
