defmodule Core.Controllers.LightController do
  @moduledoc false

  #  @behaviour Core.Controllers.Controller

  @callback set_fill() :: any
  @callback turn_on() :: any
  @callback turn_off() :: any

  alias DB.{Light, Dimmer, Port}
  alias UiWeb.DashboardChannel.Helper, as: Channel
  alias Core.Controllers.BasicController
  alias Core.Controllers.DimmerController
  import Core.Controllers.BasicController, only: [flatten_result: 1, prepare_for_basic: 1]

  def turn_on(lights) do
    set(lights, true)
  end

  def turn_off(lights) do
    set(lights, false)
  end

  def set(lights, state, deep \\ true) do
    lights
    |> prepare_for_basic()
    |> BasicController.set(state)
    |> case do
      :ok ->
        case set_dimmer(deep, lights, state) do
          :ok -> 
            Light.update(lights)
            Enum.each(lights, fn %{id: id, ref: ref} -> Channel.broadcast_change("light", id, ref + 1) end)
            :ok
          err -> err
        end
      err ->
        err
    end
  end

  def set_brightness(lights, brightness) when is_list(lights) do
    lights
    |> Enum.map(fn light -> light.dimmer end)
    |> Enum.uniq()
    |> Enum.map(fn dimmer -> DimmerController.set_brightness(dimmer, brightness, false) end)
    |> flatten_result()
  end

  # Privates

  defp split_types(lights) do
    lights
    |> DB.Repo.preload(dimmer: [:port])
    |> Enum.split_with(&Light.dim_light?/1)
  end

  defp get_dimmers(lights) do
    lights
    |> DB.Repo.preload(dimmer: [:port])
    |> Enum.map(fn l -> l.dimmer end)
    |> Enum.filter(fn d -> d != nil end)
    |> Enum.uniq()
  end

  defp set_dimmer(true, lights, state) do
    fill = if state, do: 100, else: 0

    get_dimmers(lights)
    |> Enum.map(fn x -> DimmerController.set_brightness(x, fill, false) end)
    |> flatten_result()
  end

  defp set_dimmer(_, _, _) do
    :ok
  end
end
