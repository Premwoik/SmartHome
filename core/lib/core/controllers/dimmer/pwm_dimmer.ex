defmodule Core.Controllers.Dimmer.PwmDimmer do
  @moduledoc false

  use Core.Controllers.DimmerController
  alias Core.Controllers.BasicController

  @impl true
  def get_state(dimmer) do
    {:ok, dimmer.fill > 0}
  end

  @impl true
  def set_state(dimmer, s) do
    fill = if(s, do: 100, else: 0)
    set_brightness(dimmer, fill)
  end

  @impl true
  def set_brightness(dimmer, fill) do
    dimmer.port
    |> DB.Repo.preload(:device)
    |> List.wrap()
    |> BasicController.set_state(fill: fill)
  end

  # Privates
end
