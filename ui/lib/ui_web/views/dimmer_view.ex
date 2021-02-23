defmodule UiWeb.DimmerView do
  use UiWeb, :view
  alias UiWeb.PortView
  import DB.Port, only: [from_more: 2]
  import UiWeb.View.Helper

  def render("index.json", %{dimmers: dimmers}) do
    render_many(dimmers, UiWeb.DimmerView, "dimmer.json")
  end

  def render("show.json", %{dimmer: dimmer}) do
    render_one(dimmer, UiWeb.DimmerView, "dimmer.json")
  end

  def render("dimmer.json", %{dimmer: dimmer}) do
    port = obj_to_view(PortView, :port, dimmer)

    dimmer =
      %{
        "@type": "dimmer",
        red: from_more(dimmer, :red),
        green: from_more(dimmer, :green),
        blue: from_more(dimmer, :blue),
        white: from_more(dimmer, :white),
        fill: from_more(dimmer, :fill),
        direction: from_more(dimmer, :direction),
        full_time: from_more(dimmer, :time)
      }
      |> add_color()

    Map.merge(port, dimmer)
  end

  def add_color(%{red: nil, green: nil, blue: nil} = o), do: o

  def add_color(%{red: r, green: g, blue: b} = o) do
    Map.put(o, :color, Tint.RGB.to_hex(Tint.RGB.new(r, g, b)))
  end
end
