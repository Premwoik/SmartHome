defmodule Core.Actions.ToggleGroup do
  @moduledoc false
  require Logger
  alias Core.Controllers.LightController
  alias Core.Controllers.DimmerController
  alias Core.Controllers.SunblindController
  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{}
  end

  @impl true
  def execute(_on_off, action, amem) do
    args = DB.Action.get_args_ids(action)
    lights = DB.Light.get_by_port(args)
    dimmers = DB.Dimmer.get_by_port(args)
    sunblinds = DB.Sunblind.get_by_port(args)

    if any_on?(lights, dimmers, sunblinds) do
      LightController.turn_off(lights)
      DimmerController.turn_off(dimmers)
      SunblindController.close(sunblinds)
    else
      LightController.turn_on(lights)
      DimmerController.turn_on(dimmers)
      SunblindController.open(sunblinds)
    end

    amem
  end

  #  Privates
  defp any_on?(lights, dimmers, sunblinds),
    do:
      Enum.any?(lights, &(&1.port.state == true)) ||
        Enum.any?(dimmers, &(&1.port.state == true)) ||
        Enum.any?(sunblinds, &(&1.state == "open"))
end
