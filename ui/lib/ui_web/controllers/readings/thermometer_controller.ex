defmodule UiWeb.ThermometerController do
  use UiWeb, :controller

  alias Ui.ThermometerAdmin, as: Admin
  alias DB.Thermometer

  action_fallback UiWeb.FallbackController

  def get_temperature(conn, %{"id" => id}) do
    data = DB.Thermometer.Read.find(id)
    render(conn, "index.json", readings: data)
  end

  def index(conn, _params) do
    thermometers = Admin.list_thermometers()
    render(conn, "index.json", thermometers: thermometers)
  end

  def create(conn, %{"thermometer" => thermometer_params}) do
    with {:ok, %Thermometer{} = thermometer} <- Admin.create_thermometer(thermometer_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.thermometer_path(conn, :show, thermometer))
      |> render("show.json", thermometer: thermometer)
    end
  end

  def show(conn, %{"id" => id}) do
    thermometer = Admin.get_thermometer!(id)
    render(conn, "show.json", thermometer: thermometer)
  end

  def update(conn, %{"id" => id, "thermometer" => thermometer_params}) do
    thermometer = Admin.get_thermometer!(id)

    with {:ok, %Thermometer{} = thermometer} <-
           Admin.update_thermometer(thermometer, thermometer_params) do
      render(conn, "show.json", thermometer: thermometer)
    end
  end

  def delete(conn, %{"id" => id}) do
    thermometer = Admin.get_thermometer!(id)

    with {:ok, %Thermometer{}} <- Admin.delete_thermometer(thermometer) do
      send_resp(conn, :no_content, "")
    end
  end
end
