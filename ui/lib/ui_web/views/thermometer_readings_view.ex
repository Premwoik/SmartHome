defmodule UiWeb.ThermometerReadingsView do
  use UiWeb, :view
  alias UiWeb.ThermometerReadingsView

  def render("index.json", %{thermometer_readings: rs}) do
    render_many(rs, ThermometerReadingsView, "thermometer_reading.json")
  end

  def render("show.json", %{thermometer_reading: r}) do
    render_one(r, ThermometerReadingsView, "thermometer_reading.json")
  end

  def render("thermometer_reading.json", %{thermometer_readings: r}) do
    %{id: r.id,
      date: r.inserted_at,
      value: r.value
    }
  end
end
