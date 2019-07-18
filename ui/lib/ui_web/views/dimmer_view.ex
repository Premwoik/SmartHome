defmodule UiWeb.DimmerView do
  use UiWeb, :view
  alias UiWeb.DimmerView
  alias UiWeb.PortView
  alias UiWeb.LightView
  alias UiWeb.View.Helper

  def render("index.json", %{dimmers: dimmers}) do
    render_many(dimmers, UiWeb.DimmerView, "dimmer.json")
  end

  def render("show.json", %{dimmer: dimmer}) do
    render_one(dimmer, UiWeb.DimmerView, "dimmer.json")
  end

  def render("dimmer.json", %{dimmer: dimmer}) do
    %{
      id: dimmer.id,
      port: PortView.render("port.json", %{port: dimmer.port}),
      port_id: dimmer.port_id,
      fill: dimmer.fill,
      direction: dimmer.direction,
      full_time: dimmer.time,
      ref: dimmer.ref,
      lights: nil, #Helper.objs_to_view(UiWeb.LightView, :light, dimmer.lights),
      '@type': "dimmer"
    }
  end
end
