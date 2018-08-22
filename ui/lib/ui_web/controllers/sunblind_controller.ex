defmodule UiWeb.SunblindController do
  @moduledoc false
  use UiWeb, :controller

  def index(conn, _params) do
    dims = DB.Dao.get_sunblinds()
    json conn |> put_status(:ok), dims
  end

  def toggle(conn, _params) do
    sunblinds = DB.Dao.get_sunblinds()
    status = !(Enum.any? sunblinds, &(&1.status == true))
    Device.Controller.set_outputs(:ard_mega, sunblinds, status)
    json conn
      |> put_status(:ok), ""
  end

  def toggle_one(conn, %{"id" => id} = params) do
    sunblind = DB.Dao.get_port(id)
    Device.Controller.set_outputs(:ard_mega, [sunblind], !sunblind.state)
    json conn
      |> put_status(:ok), ""
  end

end
