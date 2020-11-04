defmodule Core.Tasks.ReadUsedEnergy do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias Core.Controllers.WattmeterController, as: Wattmeters
  require Logger

#  @device Application.get_env(:core, :device_helper)
#  @actions Application.get_env(:core, :actions_server)

  @device Core.Device
  @actions Core.Actions

  @impl true
  def execute(task, _) do
    Wattmeters.read(task.device)
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
