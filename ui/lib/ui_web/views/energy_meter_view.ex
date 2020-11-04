defmodule UiWeb.EnergyMeterView do
  use UiWeb, :view
  alias UiWeb.{EnergyMeterView, MeterReadingsView}

  def render("index.json", %{wattmeters: wattmeters}) do
    render_many(wattmeters, EnergyMeterView, "energy_meter.json")
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
      '@type': "thermometer"
    }
  end
end
