defmodule UiWeb.ThermometerView do
  use UiWeb, :view
  alias UiWeb.ThermometerView
  alias UiWeb.View.Helper

  def render("index.json", %{thermometers: thermometers}) do
    render_many(thermometers, ThermometerView, "thermometer.json")
  end

  def render("show.json", %{thermometer: thermometer}) do
    render_one(thermometer, ThermometerView, "thermometer.json")
  end

  def render("thermometer.json", %{thermometer: thermometer}) do
    %{id: thermometer.id,
      name: thermometer.name,
      address: thermometer.address,
      device_id: thermometer.device_id,
      readings: Helper.objs_to_view(UiWeb.ThermometerReadingsView, :thermometer_reading, thermometer.readings),
      ref: thermometer.ref,
      '@type': "thermometer"
    }
  end
end
