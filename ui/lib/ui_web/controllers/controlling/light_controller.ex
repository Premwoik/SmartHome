defmodule UiWeb.LightController do
  use UiWeb, :controller

  alias Ui.LightAdmin, as: Admin
  alias Core.Controllers.LightController, as: Controller
  alias DB.Port, as: Light
  alias Core.Device.Static.Response

  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    lights = Admin.list_lights()
    render(conn, "index.json", lights: lights)
  end

  def create(conn, %{"light" => light_params}) do
    with {:ok, %Light{} = light} <- Admin.create_light(light_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.light_path(conn, :show, light))
      |> render("show.json", light: light)
    end
  end

  def show(conn, %{"id" => id}) do
    light = Admin.get_light!(id)
    render(conn, "show.json", light: light)
  end

  def update(conn, %{"id" => id, "light" => light_params}) do
    light = Admin.get_light!(id)

    with {:ok, %Light{} = light} <- Admin.update_light(light, light_params) do
      render(conn, "show.json", light: light)
    end
  end

  def delete(conn, %{"id" => id}) do
    light = Admin.get_light!(id)

    with {:ok, %Light{}} <- Admin.delete_light(light) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", true))
  end

  def set_off(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", false))
  end

  def set(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, light} <- Admin.get_light(id),
         true <- DB.check_ref(o, light),
         %Response{ok: [light]} <- Controller.set_state([light], state: state) do
      render(conn, "show.json", light: light)
    end
  end
end
