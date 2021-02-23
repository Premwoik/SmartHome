defmodule UiWeb.LightView do
  use UiWeb, :view
  alias UiWeb.{PortView, LightView}
  import DB.Port, only: [from_more: 2]
  import UiWeb.View.Helper

  def render("index.json", %{lights: lights}) do
    render_many(lights, LightView, "light.json")
  end

  def render("show.json", %{light: light}) do
    render_one(light, LightView, "light.json")
  end

  def render("light.json", %{light: light}) do
    port = obj_to_view(PortView, :port, light)
    light = %{"@type": "light", dimmer_id: from_more(light, :dimmer_id) |> foreign_view()}
    Map.merge(port, light)
  end
end
