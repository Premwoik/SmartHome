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
      arguments: encode_arguments(action.arguments),
      frequency: action.timeout,
      start_time: action.start_time,
      end_time: action.end_time,
      port_id: foreign_view(action.port_id),
      ref: action.ref,
      "@type": "action"
    }
  end

  def encode_arguments(arguments) do
    up_down = Keyword.get(arguments, :up_down, [])
    up = Keyword.get(arguments, :up, [])
    down = Keyword.get(arguments, :down, [])
    %{up_down: foreign_view(up_down), up: foreign_view(up), down: foreign_view(down)}
  end

  def foreign_view(foreigns) when is_list(foreigns),
    do: Enum.map(foreigns, &foreign_view/1)

  def foreign_view({:foreign, mod, id}) do
    %{"@type": "foreign", module: mod, id: id}
  end

  def foreign_view(f), do: f
end
