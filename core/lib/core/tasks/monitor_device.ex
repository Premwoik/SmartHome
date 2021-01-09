defmodule Core.Tasks.MonitorDevice do
  @moduledoc false

  @behaviour Core.Tasks.Task

  require Logger

  import Core.Tasks.ReadOutputs, only: [handle_outputs: 3]
  import Core.Tasks.ReadInputs, only: [handle_inputs: 3]

  alias Core.Device

  @impl true
  def execute(%DB.Task{device: device}, state) do
    with {:ok, {inputs, outputs}} <- Device.do_(:check_for_changes, device),
         {:ok, state} <- handle_inputs(device, inputs, state),
         {:ok, state} <- handle_outputs(device, outputs, state) do
      {:ok, state}
    end
  end

  @impl true
  def init_state() do
    %{last_inputs: [], last_outputs: []}
  end
end
