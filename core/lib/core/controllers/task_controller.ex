defmodule Core.Controllers.TaskController do
  @moduledoc false
  @behaviour Core.Controllers.Controller
  alias UiWeb.DashboardChannel.Helper, as: Channel

  def turn_on(tasks) do
    set(tasks, "waiting")
  end

  def turn_off(tasks) do
    set(tasks, "inactive")
  end

  def set(tasks, status) do
    res =
      tasks
      |> Enum.map(fn x -> x.id end)
      |> DB.Task.update_status(status)

    Core.Tasks.update()
    Enum.each(tasks, fn %{id: id, ref: ref} -> Channel.broadcast_change("task", id, ref + 1) end)
    res
  end
end
