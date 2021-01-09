defmodule Core.Controllers.ActionController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel

  defmodule Activate do
    use Core.Controllers.IOBeh
    alias Core.Controllers.IOBeh
    alias Core.Actions

    @impl IOBeh
    def turn_on(actions, _ops) do
      actions = Enum.map(actions, fn x -> x.id end)
      Actions.activate_up(actions)
    end

    @impl IOBeh
    def turn_off(actions, _ops) do
      actions = Enum.map(actions, fn x -> x.id end)
      Actions.activate_down(actions)
    end

    @impl IOBeh
    def toggle(actions, _ops) do
      actions = Enum.map(actions, fn x -> x.id end)
      Actions.activate_up(actions)
    end
  end

  @impl IOBeh
  def turn_on(actions, _ops) do
    set(actions, true)
  end

  @impl IOBeh
  def turn_off(actions, _ops) do
    set(actions, false)
  end

  # Privates

  defp set(actions, state) do
    res =
      actions
      |> Enum.map(fn a -> a.id end)
      |> DB.Action.change_state(state)

    Core.Actions.reload_actions()

    Enum.each(actions, fn %{id: id, ref: ref} ->
      Channel.broadcast_item_change("action", id, ref + 1)
    end)

    res
  end
end
