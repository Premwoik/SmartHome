defmodule Core.Tasks.Heartbeat do
  @moduledoc false
  @behaviour Core.Tasks.Task
  alias Core.Devices
  require Logger

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    {time, res} = Benchmark.measure_r(fn -> @device.do_(:heartbeat, task.device) end)

    case res do
      {:ok, _} ->
        Logger.debug("Heartbeat to #{inspect(task.device.name)} passed with time: #{time}sec.")

      err ->
        Logger.error(
          "Heartbeat to #{inspect(task.device.name)} failed with result #{inspect(err)}."
        )
    end

    :ok
  end

  @impl true
  def init_state() do
    :empty
  end
end
