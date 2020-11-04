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
      ref: action.ref,
      '@type': "action"
    }
  end


  def render("show_args.json", %{args: args}) do
    render_many(args, ActionView, "arg.json")
  end

  def render("arg.json", %{action: arg}) do
    %{
      action_id: arg.id,
      port_id: arg.port_id,
      name: arg.name,
      type: arg.type
    }
  end

  def render("show_items.json", %{items: items}) do
    render_many(items, ActionView, "action_item.json")
  end

  def render("action_item.json", %{action: i}) do
    type = case i do
      %DB.Port{} -> :port
      %DB.Sunblind{} -> :sunblind
      %DB.Dimmer{} -> :dimmer
      %DB.Light{} -> :light
    end
    typeStr = to_string(type)
    module = String.to_atom("Elixir.UiWeb."<>upcaseFirst(typeStr)<>"View")
    module.render(to_string(type) <> ".json", %{type => i})
  end
  def upcaseFirst(<<first :: utf8, rest :: binary>>), do: String.upcase(<<first :: utf8>>) <> rest

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
