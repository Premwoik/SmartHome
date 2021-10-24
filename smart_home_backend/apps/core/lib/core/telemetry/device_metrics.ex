defmodule Core.Telemetry.DeviceMetrics do
  require Logger

  import Core.Telemetry.Helpers

  alias DB.Stats.DeviceJournal, as: DJ
  alias Core.Device.Static.Response

  def make_report(
        [:core, :device, :do],
        %{duration: _, result: res, args: _} = mesurements,
        %{function: fun, device: device} = tags,
        nil
      ) do
    safe_code(fn ->
      Response.on_success(res, fn _ok ->
        log_operation(mesurements, tags)
      end)

      Response.on_error(res, fn _err ->
        log_error(mesurements, tags)
      end)

      #DJ.log_use(device.id, res, fun)
    end)
  end

  defp log_operation(%{duration: dur, args: args}, %{function: fun, device: device})
       when fun in [:set_outputs, :set_brightness, :set_white_brightness, :set_color] do
    ports =
      args
      |> List.flatten()
      |> pretty_args(fun)

    Logger.info(
      "Call [operation=#{Atom.to_string(fun)}, device=#{device.name}] in [time=#{dur}s] with [status=ok, ports=#{inspect(ports)}]",
      ansi_color: :green
    )
  end

  defp log_operation(_, _) do
    :ok
  end

  defp pretty_args(args, :set_outputs) do
    args
    |> Enum.map(fn %{name: name, state: %{"value" => val}} -> {name, val} end)
  end

  defp pretty_args(args, :set_brightness) do
    args
    |> Enum.map(fn %{name: name, state: %{"brightness" => val}} -> {name, val} end)
  end

  defp pretty_args(args, :set_white_brightness) do
    args
    |> Enum.map(fn %{name: name, state: %{"white" => val}} -> {name, val} end)
  end

  defp pretty_args(args, :set_color) do
    args
    |> Enum.map(fn %{name: name, state: %{"color" => val}} -> {name, val} end)
  end

  defp pretty_args(args, _) do
    args
    |> Enum.map(fn %{name: name} -> name end)
  end

  defp log_error(%{duration: d}, %{device: device, function: func}) do
    Logger.error(
      "Call [operation=#{Atom.to_string(func)}, name=#{device.name}] in [time=#{d}s] with [status=error]"
    )
  end

  defp log_error(_, _) do
    :ok
  end
end
