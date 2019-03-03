defmodule UiWeb.PortController do
  use UiWeb, :controller

  alias Ui.PortAdmin
  alias DB.Port
  alias Core.Controllers.BasicController

  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    ports = PortAdmin.list_ports()
    render(conn, "index.json", ports: ports)
  end

  def create(conn, %{"port" => port_params}) do
    with {:ok, %Port{} = port} <- PortAdmin.create_port(port_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.port_path(conn, :show, port))
      |> render("show.json", port: port)
    end
  end

  def show(conn, %{"id" => id}) do
    port = PortAdmin.get_port!(id)
    render(conn, "show.json", port: port)
  end

  def update(conn, %{"id" => id, "port" => port_params}) do
    port = PortAdmin.get_port!(id)

    with {:ok, %Port{} = port} <- PortAdmin.update_port(port, port_params) do
      render(conn, "show.json", port: port)
    end
  end

  def delete(conn, %{"id" => id}) do
    port = PortAdmin.get_port!(id)

    with {:ok, %Port{}} <- PortAdmin.delete_port(port) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => id} = o) do
    DB.Port.get([id])
    |> BasicController.turn_on()

    DashHelper.broadcast_update_from(o, [id], "port")

    port = PortAdmin.get_port!(id)
    render(conn, "show.json", port: port)
  end

  def set_off(conn, %{"id" => id} = o) do
    DB.Port.get([id])
    |> BasicController.turn_off()

    DashHelper.broadcast_update_from(o, [id], "port")

    port = PortAdmin.get_port!(id)
    render(conn, "show.json", port: port)
  end

end
