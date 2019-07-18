defmodule Core.Controllers.ActionController do
  @moduledoc false

  @behaviour Core.Controllers.Controller
  alias UiWeb.DashboardChannel.Helper, as: Channel

  def turn_on(actions) do
    set(actions, true)
  end

  def turn_off(actions) do
    set(actions, false)
  end

  def set(actions, state) do
    res =
      actions
      |> Enum.map(fn a -> a.id end)
      |> DB.Action.change_state(state)

    Core.Actions.reload_actions()
    Enum.each(actions, fn %{id: id, ref: ref} -> Channel.broadcast_change("action", id, ref + 1) end)
    res
  end
end
