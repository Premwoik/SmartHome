defmodule UiWeb.MeterReadingsView do
  use UiWeb, :view
  alias UiWeb.MeterReadingsView

  def render("index.json", %{meter_readings: rs}) do
    render_many(rs, MeterReadingsView, "meter_reading.json")
  end

  def render("show.json", %{meter_reading: r}) do
    render_one(r, MeterReadingsView, "meter_reading.json")
  end

  def render("meter_reading.json", %{meter_readings: r}) do
    %{id: r.id,
      date: r.inserted_at,
      value: r.value
    }
  end
end
