defmodule Core.Controllers.Dimmer.PwmDimmer do
  @moduledoc false

  use Core.Controllers.DimmerBeh
  alias Core.Controllers.BasicController

  @impl true
  def set_state(dimmer, state: s) do
    fill = if(s, do: 100, else: 0)
    set_brightness(dimmer, fill)
  end

  @impl true
  def set_brightness(dimmer, fill: fill) do
    BasicController.set_state([dimmer], fill: fill)
  end

  # Privates
end
