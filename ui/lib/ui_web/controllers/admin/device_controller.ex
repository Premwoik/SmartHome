defmodule UiWeb.DeviceController do
  use UiWeb, :controller

  alias Ui.Admin
  alias DB.Device

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    devices = Admin.list_devices()
    render(conn, "index.json", devices: devices)
  end

  def create(conn, %{"device" => device_params}) do
    with {:ok, %Device{} = device} <- Admin.create_device(device_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.device_path(conn, :show, device))
      |> render("show.json", device: device)
    end
  end

  def show(conn, %{"id" => id}) do
    device = Admin.get_device!(id)
    render(conn, "show.json", device: device)
  end

  def update(conn, %{"id" => id, "device" => device_params}) do
    device = Admin.get_device!(id)

    with {:ok, %Device{} = device} <- Admin.update_device(device, device_params) do
      render(conn, "show.json", device: device)
    end
  end

  def delete(conn, %{"id" => id}) do
    device = Admin.get_device!(id)

    with {:ok, %Device{}} <- Admin.delete_device(device) do
      send_resp(conn, :no_content, "")
    end
  end
end
