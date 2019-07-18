defmodule UiWeb.TaskView do
  use UiWeb, :view
  alias UiWeb.TaskView
  alias UiWeb.View.Helper

  def render("index.json", %{tasks: tasks}) do
    render_many(tasks, TaskView, "task.json")
  end

  def render("show.json", %{task: task}) do
    render_one(task, TaskView, "task.json")
  end

  def render("task.json", %{task: task}) do
    %{
      id: task.id,
      name: task.name,
      type_id: task.type_id,
      # TODO do something with type
      type: nil, # Helper.obj_to_view(TaskView, :type, task.type),
      status: task.status,
      action_id: task.action_id,
      device_id: task.device_id,
      frequency: task.frequency,
      execution_time: task.execution_time,
      limit: task.limit,
      start_date: task.start_date,
      end_date: task.end_date,
      ref: task.ref,
      '@type': "task"
    }
  end

  # def render("show.json", %{dash_task: task}) do
  # %{data: render_one(task, TaskView, "dash_task.json")}
  # end

  # def render("dash_task.json", %{task: task}) do
  # %{id: task.id,
  # name: task.name,
  # type: task.type.name,
  # status: task.status,
  # task: ""
  # }
  # end
end
