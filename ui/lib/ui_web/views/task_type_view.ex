defmodule UiWeb.TaskTypeView do
  use UiWeb, :view
  alias UiWeb.TaskTypeView

  def render("index.json", %{task_types: tasks}) do
    render_many(tasks, TaskTypeView, "task_type.json")
  end

  def render("show.json", %{task_type: t}) do
    render_one(t, TaskTypeView, "task_type.json")
  end

  def render("task_type.json", %{task_type: t}) do
    %{
      id: t.id,
      name: t.name,
      module: t.module,
      action: t.action,
      device: t.device
    }
  end
end
