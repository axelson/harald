defmodule Harald.Spec do
  @moduledoc """
  Entry point to generate function clauses to serialize Bluetooth HCI data.
  """

  alias Harald.{HCI, HCI.Event, Spec.Parser}
  require Harald.Spec.Generator, as: Generator

  @type t :: %{
          commands: %{required(HCI.ogf()) => %{required(HCI.ocf()) => command()}},
          events: %{required(Event.event_code()) => event()}
        }

  @type command :: %{
          command: String.t(),
          parameters: [parameter()],
          return: []
        }

  @type parameter :: %{name: String.t(), size: pos_integer(), type: parameter_type()}

  @type parameter_type ::
          :boolean
          | :error_code
          | :integer
          | :opcode
          | :address_type
          | :advertising_pdu
          | :null_terminated

  @type event :: %{event: String.t(), parameters: [parameter()]}

  @callback deserialize(binary()) :: {:ok, map()} | {:error, binary() | map()}
  @callback serialize(map()) :: {:ok, binary()} | :error

  @doc """
  Returns the processed Bluetooth spec.
  """
  def get_processed() do
    case GenServer.whereis(unquote(__MODULE__)) do
      nil ->
        bt_spec = from_priv("core_v5_1.exs")
        spec = Parser.parse(bt_spec)
        Agent.start(fn -> spec end, name: __MODULE__)
        spec

      pid ->
        Agent.get(pid, fn spec -> spec end)
    end
  end

  defmacro define_serializers, do: Generator.define_serializers(get_processed())

  defmacro define_generators, do: Generator.define_generators(get_processed())

  defp from_priv(path) do
    :harald
    |> :code.priv_dir()
    |> Path.join(path)
    |> Code.eval_file()
    |> elem(0)
    |> atomize()
  end

  # recursively walks an enumerable looking for binaries to downcase into atoms
  defp atomize(%{} = map), do: Enum.into(map, %{}, &atomize/1)
  defp atomize({k, v}) when is_binary(v), do: {k, atomize(v)}
  defp atomize({k, %Range{} = v}), do: {k, v}
  defp atomize({k, %{} = v}), do: {k, atomize(v)}
  defp atomize({k, [h | t]}), do: {k, [atomize(h) | atomize(t)]}
  defp atomize({k, v}), do: {k, v}
  defp atomize([]), do: []
  defp atomize([h | t]), do: [atomize(h) | atomize(t)]

  defp atomize(bin) when is_binary(bin) do
    bin
    |> String.replace(" ", "_")
    |> String.downcase()
    |> String.to_atom()
  end

  defp atomize(pass_through), do: pass_through
end
