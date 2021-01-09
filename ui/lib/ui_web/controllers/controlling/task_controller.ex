defmodule UiWeb.TaskController do
  use UiWeb, :controller

  alias Ui.TaskAdmin, as: Admin
  alias DB.{Task, TaskType}
  alias Core.Controllers.TaskController, as: Controller

  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    tasks = Admin.list_tasks()
    render(conn, "index.json", tasks: tasks)
  end

  def create(conn, %{"task" => task_params}) do
    with {:ok, %Task{} = task} <- Admin.create_task(task_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.task_path(conn, :show, task))
      |> render("show.json", task: task)
    end
  end

  def show(conn, %{"id" => id}) do
    task = Admin.get_task!(id)
    render(conn, "show.json", task: task)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    task = Admin.get_task!(id)
    Core.Tasks.update()

    with {:ok, %Task{} = task} <- Admin.update_task(task, task_params) do
      render(conn, "show.json", task: task)
    end
  end

  def delete(conn, %{"id" => id}) do
    task = Admin.get_task!(id)

    with {:ok, %Task{}} <- Admin.delete_task(task) do
      send_resp(conn, :no_content, "")
    end
  end

  def get_types(conn, _params) do
    types = TaskType.all_map()
    json(conn, types)
  end

  def set_on(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "status", "waiting"))
  end

  def set_off(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "status", "inactive"))
  end

  def set(conn, %{"id" => id, "status" => status} = o) do
    with {:ok, task} <- Admin.get_task(id),
         true <- DB.check_ref(o, task),
         {1, nil} <- Controller.set([task], status) do
      data = Admin.get_task!(id)
      render(conn, "show.json", task: data)
    end
  end
end
