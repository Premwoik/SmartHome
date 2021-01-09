defmodule Core.Actions.AutoLights do
  @moduledoc false
  require Logger
  alias Core.Controllers.LightController
  alias Core.Controllers.DimmerController
  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{lastInvoke: 0, offPid: nil}
  end

  @impl true
  def execute(_on_off, action, %{offPid: pid} = amem) do
    Benchmark.measure_p(fn ->
      case alive?(pid) do
        true ->
          send(pid, :notified)
          amem

        false ->
          turn_on_lights(action)
          |> update_pid(amem)
      end
    end)
  end

  #  Privates

  defp alive?(nil), do: false
  defp alive?(pid), do: Process.alive?(pid)

  defp turn_on_lights(action) do
    args = DB.Action.get_args_ids(action)
    lights = DB.Light.get_by_port(args)
    dimmers = DB.Dimmer.get_by_port(args)

    if any_on?(lights, dimmers) do
      nil
    else
      res = [
        LightController.turn_on(lights)
        | Enum.map(dimmers, &DimmerController.set_brightness(&1, 100))
      ]

      case Enum.all?(res, &(&1 == :ok)) do
        true ->
          lights_ = DB.Light.get_by_port(args)
          dimmers_ = DB.Dimmer.get_by_port(args)

          [time | _] = Poison.decode!(action.params)

          {:ok, pid} = Task.start(fn -> turn_off_after(lights_, dimmers_, time) end)
          pid

        false ->
          nil
      end
    end
  end

  defp turn_off_after(lights, dimmers, time) do
    receive do
      :notified ->
        Logger.info("Turning lights delayed")
        turn_off_after(lights, dimmers, time)
    after
      time ->
        Logger.info("Lights turned off")
        LightController.turn_off(lights)
        Enum.each(dimmers, &DimmerController.set_brightness(&1, 0))
    end
  end

  defp update_pid(nil, mem), do: mem

  defp update_pid(pid, mem) do
    Map.put(mem, :offPid, pid)
  end

  defp any_on?(lights, dimmers),
    do: Enum.any?(lights, &(&1.port.state == true)) || Enum.any?(dimmers, &(&1.fill > 0))
end
