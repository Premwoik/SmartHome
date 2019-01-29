defmodule UiWeb.LightView do
  use UiWeb, :view
  alias UiWeb.{PortView, DimmerView, LightView}
  alias UiWeb.View.Helper

  def render("index.json", %{lights: lights}) do
    %{data: render_many(lights, LightView, "light.json")}
  end

  def render("show.json", %{light: light}) do
    %{data: render_one(light, LightView, "light.json")}
  end

  def render("light.json", %{light: light}) do
    IO.inspect(light)
    %{
      id: light.id,
      port_id: light.port_id,
      dimmer_id: light.dimmer_id,
      port: Helper.obj_to_view(PortView, :port, light.port),
      dimmer: Helper.obj_to_view(DimmerView, :dimmer, light.dimmer)
    }
  end

  # def render("show.json", %{dash_light: light}) do
  # %{data: render_one(light, LightView, "dash_light.json")}
  # end

  # def render("dash_light.json", %{light: light}) do
  # %{id: light.id,
  # name: light.port.name,
  # number: light.port.number,
  # state: light.port.state,
  # device_id: light.port.device_id,
  # dimmer_id: light.dimmer_id,
  # light: ""
  # }
  # end
end
