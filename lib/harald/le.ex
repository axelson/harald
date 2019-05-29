defmodule Harald.LE do
  @moduledoc """
  A collection of high level functions for working with BLE (Bluetooth Low Energy) functionality.
  Process.exit(GenServer.whereis(String.to_atom("#{:bt}.#{Harald.LE}")), :kill)
  """

  use GenServer
  alias Harald.HCI.Event.{CommandComplete, LEMeta, LEMeta.AdvertisingReport}
  alias Harald.{HCI.Command.LEController, Transport, Transport.Handler}

  @behaviour Handler

  defmodule State do
    @moduledoc false
    defstruct devices: %{}, from: nil
  end

  @impl GenServer
  def init(_args) do
    {:ok, %State{}}
  end

  @impl Handler
  def setup(args) do
    GenServer.start_link(__MODULE__, args, name: name(args[:namespace]))
  end

  @doc """
  Initiates a scan.

  Note this function does not return in `:time` ms. It returns after the Command Complete Event is
  received for the scan disable command.

  ## Options

  `:time` - how many milliseconds to wait for between receiving the Command Complete Event for the
  scan enable command and sending the scan disable command
  """
  def scan(namespace, opts \\ []) do
    time = Keyword.get(opts, :time, 5_000)
    padding = 5_000
    ret = GenServer.call(name(namespace), {:scan, namespace, time}, time + padding)
    {:ok, ret}
  end

  @impl GenServer
  def handle_call({:scan, ns, timeout}, from, %{from: nil} = state) do
    :ok = Transport.send_binary(ns, LEController.set_enable_scan(true, true))
    Process.send_after(self(), {:stop_scan, ns, from}, timeout)
    {:noreply, %{state | from: from}}
  catch
    :exit, {:timeout, _} -> {:reply, {:error, :timeout}, state}
  end

  @impl GenServer
  def handle_info(
        {:bluetooth_event, %LEMeta{subevent: %AdvertisingReport{devices: devices}}},
        state
      ) do
    state =
      Enum.reduce(devices, state, fn device, state ->
        put_device(device.address, device, state)
      end)

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:bluetooth_event, %CommandComplete{data: 0, opcode: {8, 12}}}, state) do
    GenServer.reply(state.from, state.devices)
    {:noreply, %{state | devices: %{}, from: nil}}
  end

  # Let other bluetooth events fall through.
  def handle_info({:bluetooth_event, _}, state) do
    {:noreply, state}
  end

  # this catchs the reply from the transport if the try/catch above for a :scan was triggered
  # by a timeout
  def handle_info({ref, :ok}, state) when is_reference(ref), do: {:noreply, state}

  def handle_info({:stop_scan, ns, from}, %State{devices: devices}) do
    :ok = Transport.send_binary(ns, LEController.set_enable_scan(false))
    GenServer.reply(from, devices)
    {:noreply, %State{}}
  end

  def handle_call({:scan, _, _}, _, state), do: {:reply, {:error, :busy}, state}

  defp name(namespace), do: String.to_atom("#{namespace}.#{__MODULE__}")

  defp put_device(address, device_report, %State{devices: devices} = state) do
    %State{state | devices: Map.put(devices, address, device_report)}
  end
end
