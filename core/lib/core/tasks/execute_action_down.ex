defmodule Core.Tasks.ExecuteActionDown do
  @moduledoc false

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    @actions.activate_down([task.action_id])
  end

  @impl true
  def init_state() do
    :empty
  end

end
