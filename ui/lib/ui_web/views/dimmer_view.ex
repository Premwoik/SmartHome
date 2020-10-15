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
      port: Helper.obj_to_view(PortView, :port, dimmer.port),
      port_id: dimmer.port_id,
      fill: dimmer.fill,
      red: dimmer.red,
      green: dimmer.green,
      blue: dimmer.blue,
      direction: dimmer.direction,
      full_time: dimmer.time,
      ref: dimmer.ref,
      lights: Helper.objs_to_view(UiWeb.LightView, :light, dimmer.lights),
      '@type': "dimmer"
    }
  end
end
