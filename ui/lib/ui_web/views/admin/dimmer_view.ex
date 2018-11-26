defmodule UiWeb.DimmerView do
  use UiWeb, :view
  alias UiWeb.DimmerView
  alias UiWeb.PortView
  alias UiWeb.LightView

  def render("index.json", %{dimmers: dimmers}) do
    %{data: render_many(dimmers, DimmerView, "dimmer.json")}
  end

  def render("show.json", %{dimmer: dimmer}) do
    %{data: render_one(dimmer, DimmerView, "dimmer.json")}
  end

  def render("dimmer.json", %{dimmer: dimmer}) do
    %{id: dimmer.id,
      port: PortView.render("port.json", %{port: dimmer.port}),
      port_id: dimmer.port_id,
      fill: dimmer.fill,
      direction: dimmer.direction,
      full_time: dimmer.time
#      group: dimmer.group
    }
  end

  def render("show.json", %{dash_dimmer: dimmer}) do
    %{data: render_one(dimmer, DimmerView, "dash_dimmer.json")}
  end

  def render("dash_dimmer.json", %{dimmer: dimmer}) do
    %{id: dimmer.id,
      name: dimmer.port.name,
      fill: dimmer.fill,
      lights: Enum.map(dimmer.lights, &(LightView.render("dash_light.json", %{light: &1}))),
      dimmer: ""
      #      group: dimmer.group
    }
  end

end
