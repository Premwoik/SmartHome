defmodule UiWeb.TaskView do
  use UiWeb, :view
  alias UiWeb.TaskView
  alias UiWeb.TaskTypeView
  alias UiWeb.View.Helper
  alias UiWeb.ActionView
  alias UiWeb.DeviceView

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
      type: Helper.obj_to_view(TaskTypeView, :task_type, task.type),
      status: task.status,
      action_id: task.action_id,
      action: Helper.obj_to_view(ActionView, :action, task.action),
      device_id: task.device_id,
      device: Helper.obj_to_view(DeviceView, :device, task.device),
      frequency: task.frequency,
      execution_time: task.execution_time,
      limit: task.limit,
      start_date: task.start_date,
      end_date: task.end_date,
      ref: task.ref,
      '@type': "task"
    }
  end

end
