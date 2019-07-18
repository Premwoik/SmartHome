defmodule Core.Controllers.DimmerController do
  @moduledoc false

  alias DB.{Light, Dimmer, Port}
  alias UiWeb.DashboardChannel.Helper, as: Channel
  alias Core.Controllers.BasicController
  alias Core.Controllers.LightController
  # import Core.Controllers.BasicController, only: [flatten_result: 1, prepare_for_basic: 1]

  def set_brightness(dimmer, value, deep \\ true) do
    dimmer.port.mode
    |> case do
      "output" ->
        set_brightness_classic(dimmer, value, deep)

      "output_pwm" ->
        set_brightness_pwm(dimmer, value)

      _ ->
        :error
    end
    |> case do
      {:ok, dir} ->
        Dimmer.update_fill(dimmer, value, dir)
        Channel.broadcast_change("dimmer", dimmer.id, dimmer.ref + 1)
        :ok

      {err, _} ->
        err

      :nothing ->
        :ok

      e ->
        e
    end
  end

  defp set_brightness_classic(dimmer, value, deep) do
    if dimmer.fill != value do
      cond do
        deep && dimmer.fill == 0 ->
          :ok = LightController.set(dimmer.lights, true, false)
          set_brightness_classic_core(dimmer, value)

        deep && value == 0 ->
          LightController.set(dimmer.lights, false, false)
          |> wrap_direction(1)

        value == 0 ->
          if Dimmer.any_light_on?(dimmer), do: :nothing, else: {:ok, 1}

        true ->
          set_brightness_classic_core(dimmer, value)
      end
    else
      :nothing
    end
  end

  defp wrap_direction(res, dir) do
    {res, dir}
  end

  defp set_brightness_classic_core(dimmer, brightness) do
    {time, dir} = Dimmer.fill_to_time(dimmer, brightness)
    %Port{DB.Repo.preload(dimmer.port, :device) | timeout: time}
    |> List.wrap()
    |> BasicController.turn_on()
    |> wrap_direction(dir)
  end

  defp set_brightness_pwm(dimmer, brightness) do
    dimmer.port
    |> DB.Repo.preload(:device)
    |> List.wrap()
    |> BasicController.pwm(brightness)
  end
end
