defmodule UiWeb.EnergyMeterView do
  use UiWeb, :view
  alias UiWeb.{EnergyMeterView, MeterReadingsView}
  alias UiWeb.View.Helper

  def render("index.json", %{energy_meters: energy_meters}) do
    render_many(energy_meters, EnergyMeterView, "energy_meter.json")
  end

  def render("show.json", %{energy_meter: energy_meter}) do
    render_one(energy_meter, EnergyMeterView, "energy_meter.json")
  end

  def render("energy_meter.json", %{energy_meter: energy_meter}) do
    %{
      id: energy_meter.id,
      device: energy_meter.device,
      name: energy_meter.name,
      address: energy_meter.address,
      readings: Helper.objs_to_view(MeterReadingsView, :meter_reading, energy_meter.readings),
      ref: energy_meter.ref,
      '@type': "energy_meter"
    }
  end
end
