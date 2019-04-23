defmodule UiWeb.SunblindView do
  use UiWeb, :view
  alias UiWeb.{PortView, SunblindView}
  alias UiWeb.View.Helper

  def render("index.json", %{sunblinds: sunblinds}) do
    render_many(sunblinds, SunblindView, "sunblind.json")
  end

  def render("show.json", %{sunblind: sunblind}) do
    render_one(sunblind, SunblindView, "sunblind.json")
  end

  def render("sunblind.json", %{sunblind: sunblind}) do
    %{id: sunblind.id,
      port: Helper.obj_to_view(PortView, :port, sunblind.port),
      port_id: sunblind.port_id,
      position: sunblind.position,
      type: sunblind.type,
      full_open_time: sunblind.full_open_time,
      direction: sunblind.direction,
      state: sunblind.state,
      '@type': "sunblind"
    }
  end
end
