defmodule UiWeb.SunblindController do
  use UiWeb, :controller

  alias Ui.SunblindAdmin, as: Admin

  alias DB.Port
  alias Core.Controllers.SunblindController, as: Controller
  alias Core.Device.Static.Response

  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    sunblinds = Admin.list_sunblinds()
    render(conn, "index.json", sunblinds: sunblinds)
  end

  def create(conn, %{"sunblind" => sunblind_params}) do
    with {:ok, %Port{} = sunblind} <- Admin.create_sunblind(sunblind_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.sunblind_path(conn, :show, sunblind))
      |> render("show.json", sunblind: sunblind)
    end
  end

  def show(conn, %{"id" => id}) do
    sunblind = Admin.get_sunblind!(id)
    render(conn, "show.json", sunblind: sunblind)
  end

  def update(conn, %{"id" => id, "sunblind" => sunblind_params}) do
    sunblind = Admin.get_sunblind!(id)

    with {:ok, %Port{} = sunblind} <- Admin.update_sunblind(sunblind, sunblind_params) do
      render(conn, "show.json", sunblind: sunblind)
    end
  end

  def delete(conn, %{"id" => id}) do
    sunblind = Admin.get_sunblind!(id)

    with {:ok, %Port{}} <- Admin.delete_sunblind(sunblind) do
      send_resp(conn, :no_content, "")
    end
  end

  # CUSTOM Endpoints

  def click(conn, %{"id" => id} = o) do
    with {:ok, sunblind} <- Admin.get_sunblind(id),
         true <- DB.check_ref(o, sunblind),
         %Response{ok: [sunblind]} <- Controller.toggle([sunblind]) do
           render(conn, "show.json", %{sunblind: sunblind})
   end
  end

  def calibrate(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, sunblind} <- Admin.get_sunblind(id),
         true <- DB.check_ref(o, sunblind),
         %Port{} = sunblind <- Controller.calibrate(sunblind, state) do
         render(conn, "show.json", %{sunblind: sunblind})
   end
  end

  #  Privates

end
