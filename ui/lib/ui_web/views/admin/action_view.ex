defmodule UiWeb.ActionView do
  use UiWeb, :view
  alias UiWeb.ActionView

  def render("index.json", %{actions: actions}) do
    render_many(actions, ActionView, "action.json")
  end

  def render("show.json", %{action: action}) do
    render_one(action, ActionView, "action.json")
  end

  def render("action.json", %{action: action}) do
    %{
      id: action.id,
      name: action.name,
      function: action.function,
      active: action.active,
      params: action.params,
      frequency: action.frequency,
      start_time: action.start_time,
      end_time: action.end_time,
      port_id: action.port_id,
      '@type': "action"
    }
  end

  # def render("show.json", %{dash_action: action}) do
  # %{data: render_one(action, ActionView, "dash_action.json")}
  # end

  # def render("dash_action.json", %{action: action}) do
  # %{id: action.id,
  # name: action.name,
  # function: action.function,
  # state: action.active,
  # action: ""
  # }
  # end
end
