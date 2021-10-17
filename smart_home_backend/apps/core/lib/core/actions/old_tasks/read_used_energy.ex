defmodule Core.Actions.ReadUsedEnergy do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.WattMeterController
  alias DB.Data.Action

  @impl true
  def execute(_on_off, action, _state) do
    Logger.error("FIXME")
    device = Action.get_device(action)

    WattMeterController.read(device)
    |> case do
      {:ok, val} ->
        Logger.info("Read energy #{inspect(val)}")

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
