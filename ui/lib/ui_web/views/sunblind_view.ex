defmodule UiWeb.SunblindView do
  use UiWeb, :view
  alias UiWeb.{PortView, SunblindView}
  import DB.Port, only: [from_more: 2]
  import UiWeb.View.Helper

  def render("index.json", %{sunblinds: sunblinds}) do
    render_many(sunblinds, SunblindView, "sunblind.json")
  end

  def render("show.json", %{sunblind: sunblind}) do
    render_one(sunblind, SunblindView, "sunblind.json")
  end

  def render("sunblind.json", %{sunblind: sunblind}) do
    port = obj_to_view(PortView, :port, sunblind)

    sunblind = %{
      open_port_id: from_more(sunblind, :open_port_id) |> foreign_view(),
      type: from_more(sunblind, :type),
      full_open_time: from_more(sunblind, :full_open_time),
      state: from_more(sunblind, :state),
      "@type": "sunblind"
    }

    Map.merge(port, sunblind)
  end
end
