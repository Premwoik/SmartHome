defmodule Core.Tasks.ReadTemperature do
  @moduledoc false
  @behaviour Core.Tasks.Task
  alias Core.Devices
  require Logger

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    @device.read_temperatures(task.device)
    |> case do
      {:ok, val} ->
        Logger.info("Read temps #{inspect val}")
      _ ->
        :error
    end
  end

  @impl true
  def init_state() do
    :empty
  end
end
