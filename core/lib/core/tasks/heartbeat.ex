defmodule Core.Tasks.Heartbeat do
  @moduledoc false
  @behaviour Core.Tasks.Task
  alias Core.Devices
  require Logger

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    Logger.info("Heartbeat to #{inspect task.device.name}")
    @device.send_heartbeat(task.device)
    :ok
  end

  @impl true
  def init_state() do
    :empty
  end

end
