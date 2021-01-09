defmodule Core.Tasks.ExecuteActionUp do
  @moduledoc false

  #  @device Application.get_env(:core, :device_helper)
  #  @actions Application.get_env(:core, :actions_server)
  #

  @behaviour Core.Tasks.Task

  @actions Core.Actions

  @impl true
  def execute(task, _) do
    @actions.activate_up([task.action_id])
  end

  @impl true
  def init_state() do
    :empty
  end
end
