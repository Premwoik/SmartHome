defmodule Core.Actions.ReadUsedEnergy do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Controllers.WattMeterController, as: WattMeters
  alias DB.Data.Action

  @impl true
  def execute(_on_off, action, _state) do
    device = Action.get_device(action)

    WattMeters.read(device)
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
