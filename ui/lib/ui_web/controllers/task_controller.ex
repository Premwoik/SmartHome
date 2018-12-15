defmodule UiWeb.TaskController do
  use UiWeb, :controller

  alias Ui.TaskAdmin
  alias DB.Task

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    tasks = TaskAdmin.list_tasks()
    render(conn, "index.json", tasks: tasks)
  end

  def create(conn, %{"task" => task_params}) do
    with {:ok, %Task{} = task} <- TaskAdmin.create_task(task_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.task_path(conn, :show, task))
      |> render("show.json", task: task)
    end
  end

  def show(conn, %{"id" => id}) do
    task = TaskAdmin.get_task!(id)
    render(conn, "show.json", task: task)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    task = TaskAdmin.get_task!(id)

    with {:ok, %Task{} = task} <- TaskAdmin.update_task(task, task_params) do
      render(conn, "show.json", task: task)
    end
  end

  def delete(conn, %{"id" => id}) do
    task = TaskAdmin.get_task!(id)

    with {:ok, %Task{}} <- TaskAdmin.delete_task(task) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => id}) do
    tasks = [TaskAdmin.get_task!(id)]
    Core.Controllers.TaskController.turn_on(tasks)

    data = TaskAdmin.get_task!(id)
    render(conn, "show.json", dash_task: data)
  end

   def set_off(conn, %{"id" => id}) do
    tasks = [TaskAdmin.get_task!(id)]
    Core.Controllers.TaskController.turn_off(tasks)

    data = TaskAdmin.get_task!(id)
    render(conn, "show.json", dash_task: data)
  end



end
