defmodule Core.Actions.CloseSunblind do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Controllers.SunblindController
  alias DB.Data.Action

  @impl true
  def init_state() do
    :empty
  end

  @impl true
  def execute(:up, action, _state) do
    Action.arguments(action, :up)
    |> SunblindController.close()

    :ok
  end

  def execute(:down, action, _state) do
    Action.arguments(action, :up)
    |> SunblindController.open()

    :ok
  end
end
