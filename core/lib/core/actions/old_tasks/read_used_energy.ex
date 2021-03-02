defmodule Core.Actions.ReadUsedEnergy do
  @moduledoc false

  require Logger
  alias Core.Controllers.WattMeterController, as: WattMeters
  @behaviour Core.Actions.Action

  #  @device Application.get_env(:core, :device_helper)
  #  @actions Application.get_env(:core, :actions_server)

  #  @device Core.Device
  #  @actions Core.Actions
  alias DB.Action

  @impl true
  def execute(_on_off, action, _state) do
    device = Action.arguments(action) |> List.first()

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
