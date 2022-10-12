defmodule Core.Actions.MonitorDevice do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  import Core.Actions.ReadOutputs, only: [handle_outputs: 3]
  import Core.Actions.ReadInputs, only: [handle_inputs: 3]

  alias DB.Data.Device

  @impl true
  def execute(_on_off, action, state) do
    device_id = action.state["device_id"]

    with {:ok, device} <- Device.find(device_id),
         {:ok, {inputs, outputs}} <- Core.Device.do_(:check_for_changes, device),
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
