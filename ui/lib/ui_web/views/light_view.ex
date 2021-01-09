defmodule UiWeb.LightView do
  use UiWeb, :view
  alias UiWeb.{PortView, DimmerView, LightView}
  alias UiWeb.View.Helper

  def render("index.json", %{lights: lights}) do
    render_many(lights, LightView, "light.json")
  end

  def render("show.json", %{light: light}) do
    render_one(light, LightView, "light.json")
  end

  def render("light.json", %{light: light}) do
    %{
      id: light.id,
      port_id: light.port_id,
      dimmer_id: light.dimmer_id,
      port: Helper.obj_to_view(PortView, :port, light.port),
      dimmer: Helper.obj_to_view(DimmerView, :dimmer, light.dimmer),
      ref: light.ref,
      "@type": "light"
    }
  end
end
