defmodule Core.Controllers.TaskController do
  @moduledoc false
  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel

  @impl IOBeh
  def turn_on(tasks, _ops) do
    set(tasks, "waiting")
  end

  @impl IOBeh
  def turn_off(tasks, _ops) do
    set(tasks, "inactive")
  end

  def set(tasks, status) do
    res =
      tasks
      |> Enum.map(fn x -> x.id end)
      |> DB.Task.update_status(status)

    Core.Tasks.update()

    Enum.each(tasks, fn %{id: id, ref: ref} ->
      Channel.broadcast_item_change("task", id, ref + 1)
    end)

    res
  end
end
