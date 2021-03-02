defmodule Core.Actions.CloseSunblind do
  @moduledoc false
  require Logger
  #  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController

  @behaviour Core.Actions.Action

  @impl true
  def init_state() do
    :empty
  end

  @impl true
  def execute(:up, action, _state) do
    DB.Action.arguments(action, :up)
    |> SunblindController.close()

    :ok
  end

  def execute(:down, action, _state) do
    DB.Action.arguments(action, :down)
    |> SunblindController.open()

    :ok
  end
end
