defmodule UiWeb.EnergyMeterController do
  use UiWeb, :controller

  alias Ui.Admin
  alias Ui.Admin.EnergyMeter

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    wattmeters = Admin.list_wattmeters()
    render(conn, "index.json", wattmeters: wattmeters)
  end

  def create(conn, %{"energy_meter" => energy_meter_params}) do
    with {:ok, %EnergyMeter{} = energy_meter} <- Admin.create_energy_meter(energy_meter_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.energy_meter_path(conn, :show, energy_meter))
      |> render("show.json", energy_meter: energy_meter)
    end
  end

  def show(conn, %{"id" => id}) do
    energy_meter = Admin.get_energy_meter!(id)
    render(conn, "show.json", energy_meter: energy_meter)
  end

  def update(conn, %{"id" => id, "energy_meter" => energy_meter_params}) do
    energy_meter = Admin.get_energy_meter!(id)

    with {:ok, %EnergyMeter{} = energy_meter} <- Admin.update_energy_meter(energy_meter, energy_meter_params) do
      render(conn, "show.json", energy_meter: energy_meter)
    end
  end

  def delete(conn, %{"id" => id}) do
    energy_meter = Admin.get_energy_meter!(id)

    with {:ok, %EnergyMeter{}} <- Admin.delete_energy_meter(energy_meter) do
      send_resp(conn, :no_content, "")
    end
  end
end
