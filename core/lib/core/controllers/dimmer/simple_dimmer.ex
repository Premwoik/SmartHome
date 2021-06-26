defmodule Core.Controllers.Dimmer.SimpleDimmer do
  @moduledoc false

  use Core.Controllers.DimmerBeh
  alias Core.Controllers.PortController
  alias Core.Broadcast
  alias DB.Port

  @impl true
  def set_state(dimmer, opts) do
    {:ok, state} = Keyword.fetch(opts, :state)
    PortController.set_state([dimmer], state)
    |> update_lights()
  end

  @impl true
  def set_brightness(dimmer, opts) do
    {:ok, fill} = Keyword.fetch(opts, :fill)
    PortController.set_fill([dimmer], fill)
  end

  # Privates
  #

  def update_lights(%{ok: [dimmer], result: [{_, {:ok, [dimmer_res]}}]} = response) do
    lights_res = Map.fetch!(dimmer_res, "lights") |> Enum.sort_by(& &1["pin"])
    lights = 
      Port.from_more(dimmer, :lights) 
      |> Enum.sort_by(& &1.number)
      |> Enum.zip(lights_res)
      |> Enum.map(fn {l, %{"state" => state}} -> 
        state = if(state == 1, do: true, else: false)
        light = Port.update(l, state: state) 
        Broadcast.broadcast_item_change(:light, light)
        light
      end)

    dimmer = Port.cast(dimmer, more: [lights: lights])
    Map.put(response, :ok, [dimmer])
  end
end
