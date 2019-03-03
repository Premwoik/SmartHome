defmodule UiWeb.LightController do
  use UiWeb, :controller

  alias Ui.LightAdmin
  alias Core.Controllers.LightController
  #alias Core.Controllers.BasicController
  alias DB.Light

  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    lights = LightAdmin.list_lights()
    render(conn, "index.json", lights: lights)
  end

  def create(conn, %{"light" => light_params}) do
    with {:ok, %Light{} = light} <- LightAdmin.create_light(light_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.light_path(conn, :show, light))
      |> render("show.json", light: light)
    end
  end

  def show(conn, %{"id" => id}) do
    light = LightAdmin.get_light!(id)
    render(conn, "show.json", light: light)
  end

  def update(conn, %{"id" => id, "light" => light_params}) do
    light = LightAdmin.get_light!(id)

    with {:ok, %Light{} = light} <- LightAdmin.update_light(light, light_params) do
      render(conn, "show.json", light: light)
    end
  end

  def delete(conn, %{"id" => id}) do
    light = LightAdmin.get_light!(id)

    with {:ok, %Light{}} <- LightAdmin.delete_light(light) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => id} = o) do
      [LightAdmin.get_light!(id)]
      |> LightController.turn_on()
      #|> BasicController.prepare_for_basic()
      #|> BasicController.turn_on()

    DashHelper.broadcast_update_from(o, [id], "light")

    light = LightAdmin.get_light!(id)
    render(conn, "show.json", light: light)
  end

  def set_off(conn, %{"id" => id} = o) do
      [LightAdmin.get_light!(id)]
      |> LightController.turn_off()
      #|> BasicController.prepare_for_basic()
      #|> BasicController.turn_off()

    DashHelper.broadcast_update_from(o, [id], "light")

    light = LightAdmin.get_light!(id)
    render(conn, "show.json", light: light)
  end

end
