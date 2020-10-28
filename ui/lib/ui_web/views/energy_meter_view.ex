defmodule UiWeb.EnergyMeterView do
  use UiWeb, :view
  alias UiWeb.EnergyMeterView

  def render("index.json", %{wattmeters: wattmeters}) do
    %{data: render_many(wattmeters, EnergyMeterView, "energy_meter.json")}
  end

  def render("show.json", %{energy_meter: energy_meter}) do
    %{data: render_one(energy_meter, EnergyMeterView, "energy_meter.json")}
  end

  def render("energy_meter.json", %{energy_meter: energy_meter}) do
    %{id: energy_meter.id,
      device: energy_meter.device,
      name: energy_meter.name,
      address: energy_meter.address,
      ref: energy_meter.ref}
  end
end
