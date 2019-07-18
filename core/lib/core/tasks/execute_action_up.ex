defmodule Core.Tasks.ExecuteActionUp do
  @moduledoc false

  @device Application.get_env(:core, :device_helper)
  @actions Application.get_env(:core, :actions_server)

  @impl true
  def execute(task, _) do
    IO.inspect NaiveDateTime.utc_now()
    @actions.activate_up([task.action_id])
  end

  @impl true
  def init_state() do
    :empty
  end

end
