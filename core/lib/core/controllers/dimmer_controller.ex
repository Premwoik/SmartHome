defmodule Core.Controllers.DimmerController do
  @moduledoc false

  alias DB.{Light, Dimmer, Port}
  alias Core.Controllers.BasicController
  alias Core.Controllers.LightController
  import Core.Controllers.BasicController, only: [flatten_result: 1, prepare_for_basic: 1]


  def turn_on(dimmers) do
    {pwm, normal} = Enum.split_with(dimmers, fn d -> d.lights == [] end)
    [
      Enum.flat_map(normal, fn d -> d.lights end)
      |> LightController.turn_on(),
      set_pwm_dimmer(pwm, true)
    ]
    |> flatten_result()
  end

  def turn_off(dimmers) do
    {pwm, normal} = Enum.split_with(dimmers, fn d -> d.lights == [] end)
    [
      Enum.flat_map(normal, fn d -> d.lights end)
      |> LightController.turn_off(),
      set_pwm_dimmer(pwm, false)
    ]
    |> flatten_result()
  end

  def set_brightness(dimmer, brightness) do
    dimmer.port.mode
    |> case do
         "output" ->
           set_brightness_classic(dimmer, brightness)
         "output_pwm" ->
           set_brightness_pwm(dimmer, brightness)
         _ ->
           :error
       end
    |> case do
         :ok ->
           Dimmer.update_fill(dimmer, brightness)
           :ok
         e -> e
       end
  end


  defp set_brightness_classic(dimmer, brightness) do
    if dimmer.fill > 0 do
      Dimmer.fill_to_time(dimmer, brightness)
      |> fn t -> %Port{DB.Repo.preload(dimmer.port, :device) | timeout: t} end.()
      |> List.wrap()
      |> BasicController.turn_on()
    else
      {:error, "Dimmer must be turned on, before changing his brightness"}
    end
  end


  defp set_brightness_pwm(dimmer, brightness) do
    dimmer.port
    |> DB.Repo.preload(:device)
    |> List.wrap()
    |> BasicController.pwm(brightness)
  end


  defp set_pwm_dimmer(dimmers, s) do
    fill = if s, do: 100, else: 0
    dimmers
    |> prepare_for_basic()
    |> BasicController.pwm(fill)
    |> case do
         :ok ->
           Enum.map(
             dimmers,
             fn d -> DB.Dimmer.update(d, fill: fill, direction: 0)
             end
           )
           :ok
         error ->
           error
       end
  end

end
