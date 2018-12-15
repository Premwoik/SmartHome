defmodule Core.Tasks.ReadUsedEnergy do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias Core.Devices
  require Logger

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    @device.read_counted_values(task.device)
    |> case do
         {:ok, val} ->
            Logger.info("Read energy #{inspect val}")
         _ ->
           :error
       end
    :ok
  end

  @impl true
  def init_state() do
    :empty
  end
end