defmodule UiWeb.MeterReadingsController do
  use UiWeb, :controller

  #  alias Ui.ThermometerAdmin, as: Admin

  action_fallback UiWeb.FallbackController

  def get_temperature(conn, %{"id" => id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)
    data = DB.Stats.Temperature.get(id, precision, from: from_date, to: to_date, limit: limit)
    render(conn, "index.json", meter_readings: data)
  end

  def get_energy(conn, %{"id" => id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)
    data = DB.Stats.Energy.get(id, precision, from: from_date, to: to_date, limit: limit)
    render(conn, "index.json", meter_readings: data)
  end
end
