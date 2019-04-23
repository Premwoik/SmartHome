defmodule Core.Controllers.LightController do
  @moduledoc false

  #  @behaviour Core.Controllers.Controller

  @callback set_fill() :: any
  @callback turn_on() :: any
  @callback turn_off() :: any

  alias DB.{Light, Dimmer, Port}
  alias Core.Controllers.BasicController
  alias Core.Controllers.DimmerController
  import Core.Controllers.BasicController, only: [flatten_result: 1, prepare_for_basic: 1]

  def turn_on(lights) do
    {dim, normal} = split_types(lights)

    [
      prepare_for_basic(normal)
      |> BasicController.turn_on(),
      turn_on_dim_lights(dim)
    ]
    |> flatten_result()
  end

  def turn_off(lights) do
    {dim, normal} = split_types(lights)

    [
      prepare_for_basic(normal)
      |> BasicController.turn_off(),
      turn_off_dim_lights(dim)
    ]
    |> flatten_result()
  end

  def set_brightness(lights, brightness) when is_list(lights) do
    lights
    |> Enum.map(fn light -> light.dimmer end)
    |> Enum.uniq()
    |> Enum.map(fn dimmer -> DimmerController.set_brightness(dimmer, brightness) end)
    |> flatten_result()
  end

  # Privates

  defp split_types(lights) do
    lights
    |> DB.Repo.preload(dimmer: [:port])
    |> Enum.split_with(&Light.dim_light?/1)
  end

  defp turn_on_dim_lights(lights) do
    # TODO blocking turning on light when dimmer is not ready
    dims =
      Enum.map(lights, & &1.dimmer)
      |> Enum.uniq()
      |> Enum.filter(&(&1.fill == 0))

    dims
    |> Kernel.++(lights)
    |> prepare_for_basic()
    |> BasicController.turn_on()
    |> case do
      :ok ->
        Dimmer.update_fill(dims)
        :ok

      {:error, p, _} = res ->
        # TODO fix - only lights are not filtered for errors
        dims
        |> Enum.filter(fn dim -> !Enum.any?(p, &(dim.port_id == &1.id)) end)
        |> Dimmer.update_fill()

        res
    end
  end

  defp turn_off_dim_lights(lights) do
    prepare_for_basic(lights)
    |> BasicController.turn_off()
    |> case do
      :ok ->
        update_dimmer_fill(lights)

      {:error, p, _} = res ->
        IO.puts("NO XD")

        lights
        |> Enum.filter(fn l -> l.port.id == p.id end)
        |> update_dimmer_fill()

        res
    end
  end

  defp update_dimmer_fill(lights) do
    Enum.map(lights, & &1.dimmer)
    |> Enum.uniq()
    |> Enum.filter(&(!Dimmer.any_light_on?(&1)))
    |> Enum.map(& &1.port_id)
    |> (fn ids ->
          Dimmer.update_fill(ids, 0, 1)
          Port.update_state(ids, false)
        end).()
  end
end
