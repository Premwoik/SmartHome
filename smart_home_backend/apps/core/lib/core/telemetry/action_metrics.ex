defmodule Core.Telemetry.ActionMetrics do
  require Logger

  import Core.Telemetry.Helpers

  def make_report(
        [:core, :action, :run],
        %{duration: _} = mesurements,
        %{action: _, mode: _} = tags,
        nil
      ) do
    safe_code(fn ->
      log(mesurements, tags)
    end)
  end

  def log(%{duration: duration}, %{action: %{module: mod} = action, mode: mode})
      when mod not in ["ReadInputs", "ReadOutputs"] do
    Logger.info(
      "Invoke action [id=#{action.id}, name=#{action.name}] in [mode=#{inspect(mode)}, time=#{duration}s]",
      ansi_color: :blue
    )
  end

  def log(_, _) do
    :ok
  end
end
