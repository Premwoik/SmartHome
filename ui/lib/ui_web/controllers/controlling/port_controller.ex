defmodule UiWeb.PortController do
  use UiWeb, :controller

  alias Ui.PortAdmin, as: Admin
  alias DB.Port
  alias Core.Controllers.BasicController, as: Controller
  alias Core.Device.Static.Response

  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    ports = Admin.list_ports()
    render(conn, "index.json", ports: ports)
  end

  def create(conn, %{"port" => port_params}) do
    with {:ok, %Port{} = port} <- Admin.create_port(port_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.port_path(conn, :show, port))
      |> render("show.json", port: port)
    end
  end

  def show(conn, %{"id" => id}) do
    port = Admin.get_port!(id)
    render(conn, "show.json", port: port)
  end

  def update(conn, %{"id" => id, "port" => port_params}) do
    port = Admin.get_port!(id)

    with {:ok, %Port{} = port} <- Admin.update_port(port, port_params) do
      render(conn, "show.json", port: port)
    end
  end

  def delete(conn, %{"id" => id}) do
    port = Admin.get_port!(id)

    with {:ok, %Port{}} <- Admin.delete_port(port) do
      send_resp(conn, :no_content, "")
    end
  end

  # CUSTOM Endpoints

  def set_on(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", true))
  end

  def set_off(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", false))
  end

  def set(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, port} <- Admin.get_port(id),
         true <- DB.check_ref(o, port),
         %Response{ok: [port]} <- Controller.set_state([port], state: state) do
        render(conn, "show.json", port: port)
      end
  end

end
