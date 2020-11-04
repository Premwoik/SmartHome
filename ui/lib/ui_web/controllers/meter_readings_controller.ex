defmodule UiWeb.MeterReadingsController do
  use UiWeb, :controller

  alias Ui.ThermometerAdmin, as: Admin
  alias DB.{Thermometer, EnergyMeter}

  action_fallback UiWeb.FallbackController

  def get_temperature(conn, %{"id" => id}) do
    data = DB.Thermometer.Read.find(id)
    render(conn, "index.json", meter_readings: data )
  end

  def get_energy(conn, %{"id" => id}) do
    data = DB.Thermometer.Read.find(id)
    render(conn, "index.json", meter_readings: data )
  end

end
