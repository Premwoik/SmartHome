defmodule UiWeb.DashboardView do
  use UiWeb, :view
  alias UiWeb.DashboardView
  alias UiWeb.View.Helper

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
      title: dashboard.title,
      order: dashboard.order,
      description: dashboard.description,
      ports: Helper.objs_to_view(UiWeb.PortView, :port, unwrap(dashboard.ports)),
      lights: Helper.objs_to_view(UiWeb.LightView, :light, unwrap(dashboard.lights)),
      dimmers: Helper.objs_to_view(UiWeb.DimmerView, :dimmer, unwrap(dashboard.dimmers)),
      sunblinds: Helper.objs_to_view(UiWeb.SunblindView, :sunblind, unwrap(dashboard.sunblinds)),
      actions: Helper.objs_to_view(UiWeb.ActionView, :action, unwrap(dashboard.actions)),
      tasks: Helper.objs_to_view(UiWeb.TaskView, :task, unwrap(dashboard.tasks)),
      devices: Helper.objs_to_view(UiWeb.DeviceView, :device, unwrap(dashboard.devices))
    }
  end

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
      number: dashboard.order
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
      title: dashboard.title,
      order: dashboard.order,
      description: dashboard.description,
      content: render_content(dashboard)
    }
  end

  defp unwrap(is) do
    Enum.map(is, fn {_, i} -> i end)
  end

  def render_content(d) do
    (render_lights(d.lights) ++
       render_dimmers(d.dimmers) ++
       render_sunblinds(d.sunblinds) ++
       render_actions(d.actions) ++
       render_ports(d.ports) ++
       render_tasks(d.tasks) ++
       render_devices(d.devices))
    |> Enum.sort_by(fn {o, _} -> o end)
    |> Enum.map(fn {_, x} -> x end)
  end

  def render_lights(lights) do
    Enum.map(lights, fn {o, l} -> {o, UiWeb.LightView.render("light.json", %{light: l})} end)
  end

  def render_dimmers(dimmers) do
    Enum.map(dimmers, fn {o, d} -> {o, UiWeb.DimmerView.render("dimmer.json", %{dimmer: d})} end)
  end

  def render_sunblinds(sunblinds) do
    Enum.map(sunblinds, fn {o, d} ->
      {o, UiWeb.SunblindView.render("sunblind.json", %{sunblind: d})}
    end)
  end

  def render_actions(actions) do
    Enum.map(actions, fn {o, d} -> {o, UiWeb.ActionView.render("action.json", %{action: d})} end)
  end

  def render_ports(ports) do
    Enum.map(ports, fn {o, d} -> {o, UiWeb.PortView.render("port.json", %{port: d})} end)
  end

  #  def render_sunblinds(sunblinds) do
  #    Enum.map(sunblinds, fn {o, d} ->
  #      {o, UiWeb.SunblindView.render("sunblind.json", %{sunblind: d})}
  #    end)
  #  end

  def render_tasks(tasks) do
    Enum.map(tasks, fn {o, d} -> {o, UiWeb.TaskView.render("task.json", %{task: d})} end)
  end

  def render_devices(devices) do
    Enum.map(devices, fn {o, d} -> {o, UiWeb.DeviceView.render("device.json", %{device: d})} end)
  end
end
