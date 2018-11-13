defmodule Core.Controllers.LightController do
  @moduledoc false

  @behaviour Core.Controllers.Controller

  alias DB.{Light, Dimmer, Port}
  alias Core.Controllers.BasicController
  import Core.Controllers.BasicController, only: [flatten_result: 1]


  def turn_on(lights) do
    {dim, normal} = Enum.split_with(lights, &Light.dim_light?/1)
    [
      prepare_for_basic(normal)
      |> BasicController.turn_on(),
      turn_on_dim_lights(dim)
    ]
    |> flatten_result()
  end

  def turn_off(lights) do
    {dim, normal} = Enum.split_with(lights, &Light.dim_light?/1)
    [
      prepare_for_basic(normal)
      |> BasicController.turn_off(),
      turn_off_dim_lights(dim)
    ]
    |> flatten_result()
  end

  def toggle(lights) do
    lights
    |> Enum.split_with(fn light -> light.port.state end)
    |> fn {off, on} -> [turn_off(off), turn_on(on)] end.()
    |> flatten_result()
  end

  def set_brightness(lights, brightness) when is_list lights do
    lights
    |> Enum.map(fn light -> light.dimmer end)
    |> Enum.uniq()
    |> Enum.map(fn dimmer -> set_brightness dimmer, brightness end)
    |> flatten_result()
  end

  def set_brightness(dimmer, brightness) do
    if dimmer.fill > 0 do
      time = Dimmer.fill_to_time(dimmer, brightness)
      if time > 0 do
        new_dimmer_port = %Port{DB.Repo.preload(dimmer.port, :device) | timeout: time}
        case BasicController.turn_on([new_dimmer_port]) do
          :ok ->
            Dimmer.update_fill(dimmer, brightness)
            :ok
          error -> error
        end
      end
    else
      {:error, "Dimmer must be turned on, before changing his brightness"}
    end
  end



  # Privates
  defp prepare_for_basic(any) when is_list any do
    any
    |> Enum.map(&(&1.port))
    |> DB.Repo.preload(:device)
  end
  defp prepare_for_basic(any) do
    any
    |> DB.Repo.preload(:device)
  end

  defp turn_on_dim_lights(lights) do
    dims =
      Enum.map(lights, &(&1.dimmer))
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
           dims
           |> Enum.filter(fn dim -> !Enum.any?(p, &(dim.port_id == &1.id)) end)
           |> Dimmer.update_fill()
           res
       end
  end

  defp turn_off_dim_lights(lights) do
    #    TODO add checking return value (another words check if send passed)
    result = prepare_for_basic(lights)
             |> BasicController.turn_off()
    update_dimmer_fill(lights)
    result
  end

  defp update_dimmer_fill(lights) do
    Enum.map(lights, &(&1.dimmer))
    |> Enum.uniq()
    |> Enum.filter(&(!Dimmer.any_light_on?(&1)))
    |> Enum.map(&(&1.port_id))
    |> fn ids ->
      Dimmer.update_fill(ids, 0, 1)
      Port.update_state(ids, false)
       end.()
  end

end
