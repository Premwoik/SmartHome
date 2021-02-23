defmodule Core.Controllers.ActionController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel
  alias DB.{ScheduleJob, Action}

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

  defmodule Job do
    @spec new(String.t(), boolean, boolean) :: any
    def new(action_id, expr, signal, extended \\ false) do
      ScheduleJob.new(
        expr: expr,
        extended: extended,
        state: signal,
        action_id: action_id
      )
      |> ScheduleJob.insert()
      |> Core.Scheduler.add_action_job()
    end

    def remove(id) do
      name = String.to_atom("db_#{id}")
      Core.Scheduler.delete_job(name)
    end

    def activate(id) do
      name = String.to_atom("db_#{id}")
      Core.Scheduler.activate_job(name)
    end

    def deactivate(id) do
      name = String.to_atom("db_#{id}")
      Core.Scheduler.deactivate_job(name)
    end
  end

  # Privates

  defp set(actions, state) do
    res = Action.update(actions, active: state)

    Core.Actions.reload_actions()

    Enum.each(actions, fn %{id: id, ref: ref} ->
      Channel.broadcast_item_change("action", id, ref + 1)
    end)

    res
  end
end
