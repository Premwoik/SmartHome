defmodule UiWeb.DashboardView do
  use UiWeb, :view
  alias UiWeb.DashboardView
  import UiWeb.View.Helper
  alias DB.{Port, Action, Device}

  def render("index.json", %{dashboards: dashboards}) do
    render_many(dashboards, DashboardView, "dashboard.json")
  end

  def render("show.json", %{dashboard: dashboard}) do
    render_one(dashboard, DashboardView, "dashboard.json")
  end

  def render("dashboard.json", %{dashboard: dashboard}) do
    %{
      id: dashboard.id,
      name: dashboard.name,
      order: dashboard.order,
      ref: dashboard.ref,
      description: dashboard.description,
      content: foreign_view(dashboard.content),
    }
  end

#  def group_conditions({:foreign, mod, _}), do: mod
#  def group_conditions(%{type: t}), do: t
#
#  def standardize_keys({key, value}) do
#    case key do
#
#    end
#  end


  def render("index.json", %{dashboards_short: dashboards}) do
    render_many(dashboards, DashboardView, "dashboard_short.json")
  end

  def render("show.json", %{dashboard_short: dashboard}) do
    render_one(dashboard, DashboardView, "dashboard_short.json")
  end

  def render("dashboard_short.json", %{dashboard: dashboard}) do
    %{
      id: dashboard.id,
      name: dashboard.name,
#      number: dashboard.order
    }
  end

  def render("index.json", %{dash_dashboards: dashboards}) do
    render_many(dashboards, DashboardView, "dash_dashboard.json")
  end

  def render("show.json", %{dash_dashboard: dashboard}) do
    render_one(dashboard, DashboardView, "dash_dashboard.json")
  end

  def render("dash_dashboard.json", %{dashboard: dashboard}) do
    %{
      id: dashboard.id,
      name: dashboard.name,
      order: dashboard.order,
      description: dashboard.description,
      content: Enum.map(dashboard.content, &match_obj_to_view/1)
    }
  end

  defp unwrap(is) do
    Enum.map(is, fn {_, i} -> i end)
  end

  defp match_obj_to_view(%Port{type: t} = i) do
    case to_string(t) do
      "light" <> _ ->
        obj_to_view(UiWeb.LightView, :light, i)
      "dimmer" <> _ ->
        obj_to_view(UiWeb.DimmerView, :dimmer, i)
      "sunblind" <> _ ->
        obj_to_view(UiWeb.SunblindView, :sunblind, i)
      _ ->
        obj_to_view(UiWeb.PortView, :port, i)
    end
  end
  defp match_obj_to_view(%Device{} = i) do
    obj_to_view(UiWeb.DeviceView, :device, i)

  end
  defp match_obj_to_view(%Action{} = i) do
    obj_to_view(UiWeb.ActionView, :action, i)
  end

end
