defmodule UiWeb.ThermometerReadingsController do
  use UiWeb, :controller

  alias Ui.ThermometerAdmin, as: Admin
  alias DB.Thermometer

  action_fallback UiWeb.FallbackController

  def get_temperature(conn, %{"id" => id}) do
    data = DB.Thermometer.Read.find(id)
    render(conn, "index.json", thermometer_readings: data )
  end

end
