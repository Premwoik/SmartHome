defmodule UiWeb.SunblindController do
  use UiWeb, :controller

  alias Ui.SunblindAdmin

  @device Application.get_env(:core, :devices_module)

  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController
  alias UiWeb.Controllers.ErrorHelper

  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    sunblinds = SunblindAdmin.list_sunblinds()
    render(conn, "index.json", sunblinds: sunblinds)
  end

  def create(conn, %{"sunblind" => sunblind_params}) do
    with {:ok, %Sunblind{} = sunblind} <- SunblindAdmin.create_sunblind(sunblind_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.sunblind_path(conn, :show, sunblind))
      |> render("show.json", sunblind: sunblind)
    end
  end

  def show(conn, %{"id" => id}) do
    sunblind = SunblindAdmin.get_sunblind!(id)
    IO.inspect(sunblind)
    render(conn, "show.json", sunblind: sunblind)
  end

  def update(conn, %{"id" => id, "sunblind" => sunblind_params}) do
    sunblind = SunblindAdmin.get_sunblind!(id)

    with {:ok, %Sunblind{} = sunblind} <- SunblindAdmin.update_sunblind(sunblind, sunblind_params) do
      render(conn, "show.json", sunblind: sunblind)
    end
  end

  def delete(conn, %{"id" => id}) do
    sunblind = SunblindAdmin.get_sunblind!(id)

    with {:ok, %Sunblind{}} <- SunblindAdmin.delete_sunblind(sunblind) do
      send_resp(conn, :no_content, "")
    end
  end

  def click(conn, %{"id" => id} = o) do
    with sunblind <- DB.Sunblind.get(id),
         true <- DB.check_ref(o, sunblind),
         :ok <- SunblindController.click(sunblind)
    do
      sun = SunblindAdmin.get_sunblind!(id)
      render(conn, "show.json", %{sunblind: sun})
    else
      casual_error ->
        ErrorHelper.handling_casual_errors(conn, casual_error)
    end
  end

  def calibrate(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, sunblind} <- SunblindAdmin.get_sunblind(id),
         true <- DB.check_ref(o, sunblind),
         :ok <- SunblindController.calibrate(sunblind, state)
    do
      sun = SunblindAdmin.get_sunblind!(id)
      render(conn, "show.json", %{sunblind: sun})
    else
      casual_error ->
        ErrorHelper.handling_casual_errors(conn, casual_error)
    end
  end

end
