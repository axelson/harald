defmodule Harald.HCI.Command.ControllerAndBaseband do
  @moduledoc """
  HCI commands for working with the controller and baseband.

  > The Controller & Baseband Commands provide access and control to various capabilities of the
  > Bluetooth hardware. These parameters provide control of BR/EDR Controllers and of the
  > capabilities of the Link Manager and Baseband in the BR/EDR Controller, the PAL in an AMP
  > Controller, and the Link Layer in an LE Controller. The Host can use these commands to modify
  > the behavior of the local Controller.

  Reference: Version 5.0, Vol 2, Part E, 7.3
  """

  alias Harald.HCI

  def command(ocf, parameters), do: HCI.command(0x03, ocf, parameters)
end
