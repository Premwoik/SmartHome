defmodule UiWeb.MeterController do
  use UiWeb, :controller

  alias Ui.MeterAdmin, as: Admin
  alias DB.Meter

  action_fallback UiWeb.FallbackController

  def get_readings(conn, %{"id" => id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)
    data = DB.Stats.Temperature.get(id, precision, from: from_date, to: to_date, limit: limit)
    render(conn, "index.json", readings: data)
  end

  def index(conn, _params) do
    meters = Admin.list_meters()
    render(conn, "index.json", meters: meters)
  end

  def all_thermometers(conn, _params) do
    meters = Admin.list_meters()
    render(conn, "index.json", meters: meters)
  end

  def all_energy_meters(conn, _params) do
    meters = Admin.list_energy_meters()
    render(conn, "index.json", meters: meters)
  end

  def create(conn, %{"meter" => meter_params}) do
    with {:ok, %Meter{} = meter} <- Admin.create_meter(meter_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.meter_path(conn, :show, meter))
      |> render("show.json", meter: meter)
    end
  end

  def show(conn, %{"id" => id}) do
    meter = Admin.get_meter!(id)
    render(conn, "show.json", meter: meter)
  end

  def update(conn, %{"id" => id, "meter" => meter_params}) do
    meter = Admin.get_meter!(id)

    with {:ok, %Meter{} = meter} <-
           Admin.update_meter(meter, meter_params) do
      render(conn, "show.json", meter: meter)
    end
  end

  def delete(conn, %{"id" => id}) do
    meter = Admin.get_meter!(id)

    with {:ok, %Meter{}} <- Admin.delete_meter(meter) do
      send_resp(conn, :no_content, "")
    end
  end
end
