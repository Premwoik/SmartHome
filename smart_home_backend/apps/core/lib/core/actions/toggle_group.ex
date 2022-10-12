defmodule Core.Actions.ToggleGroup do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Controller
  alias DB.Data.Action
  alias DB.Data.Port

  @impl true
  def init_state() do
    :empty
  end

  @impl true
  def execute(_on_off, action, _state) do
    ports = Action.get_ports(action, "ports")

    if Port.any_on?(ports) do
      Controller.turn_off(ports)
    else
      Controller.turn_on(ports)
    end

    :ok
  end

  #  Privates
end
