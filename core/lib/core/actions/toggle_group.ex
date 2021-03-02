defmodule Core.Actions.ToggleGroup do
  @moduledoc false
  require Logger
  alias Core.Controller
  alias DB.{Action, Port}
  @behaviour Core.Actions.Action

  @impl true
  def init_state() do
    :empty
  end

  @impl true
  def execute(_on_off, action, _state) do
    args = Action.arguments(action) |> Enum.group_by(& &1.__struct__)
    ports = Map.get(args, DB.Port, [])

    if Port.any_on?(ports) do
      Controller.turn_off(ports)
    else
      Controller.turn_on(ports)
    end

    :ok
  end

  #  Privates
end
