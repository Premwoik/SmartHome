defmodule UiWeb.DashboardView do
  use UiWeb, :view
  alias UiWeb.DashboardView
  alias Ui.{LightView}

  def render("index.json", %{dashboards: dashboards}) do
    %{data: render_many(dashboards, DashboardView, "dashboard.json")}
  end

  def render("show.json", %{dashboard: dashboard}) do
    %{data: render_one(dashboard, DashboardView, "dashboard.json")}
  end

  def render("dashboard.json", %{dashboard: dashboard}) do
    %{id: dashboard.id,
      name: dashboard.name,
      title: dashboard.title,
      order: dashboard.order,
      description: dashboard.description,
      ports: "#{inspect dashboard.ports}",
      actions: "#{inspect dashboard.actions}",
      lights: "#{inspect dashboard.lights}",
      dimmers: "#{inspect dashboard.dimmers}",
      tasks: "#{inspect dashboard.tasks}",
      devices: "[]",
      sunblinds: "#{inspect dashboard.sunblinds}"
    }
  end

  def render("index.json", %{dashboards_short: dashboards}) do
    %{data: render_many(dashboards, DashboardView, "dashboard_short.json")}
  end

  def render("show.json", %{dashboard_short: dashboard}) do
    %{data: render_one(dashboard, DashboardView, "dashboard_short.json")}
  end

  def render("dashboard_short.json", %{dashboard: dashboard}) do
    %{id: dashboard.id,
      name: dashboard.name,
      number: dashboard.order
    }
  end


  def render("index.json", %{dash_dashboards: dashboards}) do
    %{data: render_many(dashboards, DashboardView, "dash_dashboard.json")}
  end

  def render("show.json", %{dash_dashboard: dashboard}) do
    %{data: render_one(dashboard, DashboardView, "dash_dashboard.json")}
  end

  def render("dash_dashboard.json", %{dashboard: dashboard}) do
    %{id: dashboard.id,
      name: dashboard.name,
      title: dashboard.title,
      order: dashboard.order,
      description: dashboard.description,
      content: render_content(dashboard)
    }
  end

  def render_content(d) do
    render_lights(d.lights)
    ++
    render_dimmers(d.dimmers)
    ++
    render_sunblinds(d.sunblinds)
    ++
    render_actions(d.actions)
    ++
    render_ports(d.ports)
    ++
    render_sunblinds(d.sunblinds)
    |> Enum.sort_by(fn {o, _} -> o end)
    |> Enum.map(fn {_, x} -> x end)
  end

  def render_lights(lights) do
    Enum.map(lights, fn {o, l} -> {o, UiWeb.LightView.render("dash_light.json", %{light: l})} end)
  end

  def render_dimmers(dimmers) do
    Enum.map(dimmers, fn {o, d} -> {o, UiWeb.DimmerView.render("dash_dimmer.json", %{dimmer: d})} end)
  end

  def render_sunblinds(sunblinds) do
    Enum.map(sunblinds, fn {o, d} -> {o, UiWeb.SunblindView.render("dash_sunblind.json", %{sunblind: d})} end)
  end

  def render_actions(actions) do
    Enum.map(actions, fn {o, d} -> {o, UiWeb.ActionView.render("dash_action.json", %{action: d})} end)
  end

  def render_ports(ports) do
    Enum.map(ports, fn {o, d} -> {o, UiWeb.PortView.render("dash_port.json", %{port: d})} end)
  end

  def render_sunblinds(sunblinds) do
    Enum.map(sunblinds, fn {o, d} -> {o, UiWeb.SunblindView.render("dash_sunblind.json", %{sunblind: d})} end)
  end

end
