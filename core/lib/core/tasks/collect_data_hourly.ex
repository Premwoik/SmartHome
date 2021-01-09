defmodule Core.Tasks.CollectDataHourly do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias DB.{DeviceActivations, InputActivations, OutputActivations}

  @impl true
  def execute(_task, _) do
    DeviceActivations.collect_previous_hour()
    InputActivations.collect_previous_hour()
    OutputActivations.collect_previous_hour()
    :ok
  end

  @impl true
  def init_state() do
    :empty
  end
end
