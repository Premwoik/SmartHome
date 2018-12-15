defmodule Core.Controllers.TaskController do
  @moduledoc false
  @behaviour Core.Controllers.Controller

  def turn_on(tasks) do
    tasks
    |> Enum.map(fn x -> x.id end)
    |> DB.Task.update_status("waiting")

    Core.Tasks.reload()
    :ok
  end
  def turn_off(tasks) do
    tasks
    |> Enum.map(fn x -> x.id end)
    |> DB.Task.update_status("inactive")

    Core.Tasks.reload()
    :ok
  end
end
