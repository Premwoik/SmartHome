defmodule Core.Controllers.DimmerController do
  @moduledoc false

  alias DB.{Light, Dimmer, Port, Device}
  alias UiWeb.DashboardChannel.Helper, as: Channel
  alias Core.Controllers.BasicController
  alias Core.Controllers.LightController
  alias Core.Device
  alias Core.Device.ShellyRGBW2

  # import Core.Controllers.BasicController, only: [flatten_result: 1, prepare_for_basic: 1]

  defmodule Brightness do
    defstruct [:red, :green, :blue, :gain]
  end
  defp process_response(response, dimmer) do
    case response do
      {:ok, %ShellyRGBW2{ison: state, red: r, blue: b, green: g, gain: fill}} ->
        #      TODO broadcast changes only if it is needed
        IO.inspect(response)
        Dimmer.changeset(dimmer, red: r, green: g, blue: b, fill: fill)
        |> DB.Repo.update()
        Port.changeset(dimmer.port, state: state)
        |> DB.Repo.update()
        DB.Device.changeset(dimmer.port.device, alive: true)
        |> DB.Repo.update()

        Channel.broadcast_change("dimmer", dimmer.id, dimmer.ref + 1)
      _ ->
        DB.Device.changeset(dimmer.port.device, alive: false)
        |> DB.Repo.update()
    end
  end

  def read(dimmer) do
    [dimmer.port.device, dimmer.port]
    |> Device.do_r(:status)
    |> process_response(dimmer)
    :ok
  end

  def set_rgb_brightness(dimmer, fill, red, green, blue) do
    %{device: dimmer.port.device, port: dimmer.port, red: red, green: green, blue: blue, fill: fill}
    |> Core.Device.do_r(:set_rgb)
    |> process_response(dimmer)
    :ok
  end

  def set_brightness(dimmer, value, deep \\ true)
  def set_brightness(
        %Dimmer{
          port: %Port{
            type: "dimmer_rgb"
          }
        } = dimmer,
        value,
        _
      ) do
    %{device: dimmer.port.device, port: dimmer.port, fill: value}
    |> Core.Device.do_r(:set_brightness)
    |> process_response(dimmer)
    :ok
  end

  def set_brightness(dimmer, value, deep) do
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
    |> Core.Device.do_r(:set_time_dimmer)
#    TODO should I here update also port state???
    |> IO.inspect()
    |> wrap_direction(dir)
  end

  defp set_brightness_pwm(dimmer, brightness) do
    dimmer.port
    |> DB.Repo.preload(:device)
    |> List.wrap()
    |> BasicController.pwm(brightness)
  end
end
