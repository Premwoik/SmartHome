defmodule Core.Actions.CloseSunblind do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.SunblindController
  alias DB.Data.Action

  @impl true
  def init_state() do
    :empty
  end

  @impl true
  def execute(:up, action, _state) do
    action
    |> Action.arguments(:up)
    |> SunblindController.close()

    :ok
  end

  def execute(:down, action, _state) do
    action
    |> Action.arguments(:down)
    |> SunblindController.open()

    :ok
  end
end
