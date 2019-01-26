defmodule UiWeb.SunblindController do
  use UiWeb, :controller

  alias Ui.AdminSunblind

  @device Application.get_env(:core, :devices_module)

  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController

  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    sunblinds = AdminSunblind.list_sunblinds()
    render(conn, "index.json", sunblinds: sunblinds)
  end

  def create(conn, %{"sunblind" => sunblind_params}) do
    with {:ok, %Sunblind{} = sunblind} <- AdminSunblind.create_sunblind(sunblind_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.sunblind_path(conn, :show, sunblind))
      |> render("show.json", sunblind: sunblind)
    end
  end

  def show(conn, %{"id" => id}) do
    sunblind = AdminSunblind.get_sunblind!(id)
    render(conn, "show.json", sunblind: sunblind)
  end

  def update(conn, %{"id" => id, "sunblind" => sunblind_params}) do
    sunblind = AdminSunblind.get_sunblind!(id)

    with {:ok, %Sunblind{} = sunblind} <- AdminSunblind.update_sunblind(sunblind, sunblind_params) do
      render(conn, "show.json", sunblind: sunblind)
    end
  end

  def delete(conn, %{"id" => id}) do
    sunblind = AdminSunblind.get_sunblind!(id)

    with {:ok, %Sunblind{}} <- AdminSunblind.delete_sunblind(sunblind) do
      send_resp(conn, :no_content, "")
    end
  end

  def click(conn, %{"id" => id} = o) do
    res =
      AdminSunblind.get_sunblind!(id)
      |> SunblindController.click()

    DashHelper.broadcast_update_from(o, [id], "sunblind")

    sun = AdminSunblind.get_sunblind!(id)
    render(conn, "show.json", %{sunblind: sun})
  end

  def calibrate(conn, %{"id" => id, "state" => state} = o) do
    AdminSunblind.get_sunblind!(id)
    |> SunblindController.calibrate(state)

    DashHelper.broadcast_update_from(o, [id], "sunblind")

    sun = AdminSunblind.get_sunblind!(id)
    render(conn, "show.json", %{sunblind: sun})
    #    with {:ok, s} <-
    #           id
    #           |> Sunblind.get()
    #           |> SunblindController.calibrate(state)
    #      do
    #      json conn
    #           |> put_status(:ok), "#{s.state}"
    #    else
    #      error -> json conn
    #                    |> put_status(:ok), "#{inspect error}"
    #    end
  end

  def dash_show(conn, %{"id" => id}) do
    sunblind = AdminSunblind.get_sunblind!(id)
    render(conn, "show.json", sunblind: sunblind)
  end
end
