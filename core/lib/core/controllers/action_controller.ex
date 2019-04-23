defmodule Core.Controllers.ActionController do
  @moduledoc false

  @behaviour Core.Controllers.Controller

  def turn_on(actions) do
    actions
#    |> Enum.filter(fn x -> x.active end)
    |> Enum.map(fn a -> a.id end)
    |> DB.Action.change_state(true)

    Core.Actions.reload_actions()
    :ok
  end

  def turn_off(actions) do
    actions
#    |> Enum.filter(fn x -> !x.active end)
    |> Enum.map(fn a -> a.id end)
    |> DB.Action.change_state(false)

    Core.Actions.reload_actions()
    :ok
  end

end
