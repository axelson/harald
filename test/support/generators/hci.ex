defmodule Harald.Generators.HCI do
  @moduledoc """
  StreamData generators for HCI.
  """

  use ExUnitProperties
  require Harald.Spec, as: Spec

  Spec.define_generators()
end
