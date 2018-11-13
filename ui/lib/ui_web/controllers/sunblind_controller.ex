defmodule UiWeb.SunblindController do
  @moduledoc false
  use UiWeb, :controller

  @device Application.get_env(:core, :devices_module)

  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController

  def index(conn, _params) do
    sunblinds = Sunblind.all()
    json conn
         |> put_status(:ok), sunblinds
  end

  #
  #  def toggle(conn, _params) do
  #    sunblinds = DB.Dao.get_sunblinds()
  #    status = !(Enum.any? sunblinds, &(&1.status == true))
  #    @device.set_outputs(:ard_mega, sunblinds, status)
  #    json conn
  #         |> put_status(:ok), ""
  #  end

  def click(conn, %{"id" => id} = params) do
    res =
      Sunblind.get(id)
      |> SunblindController.click()


    [data] = Sunblind.get_view_format(id)
    json conn, data
  end

  def calibrate(conn, %{"id" => id, "state" => state}) do
    with {:ok, s} <-
           id
           |> Sunblind.get()
           |> SunblindController.calibrate(state)
      do
      json conn
           |> put_status(:ok), "#{s.state}"
    else
      error -> json conn
                    |> put_status(:ok), "#{inspect error}"
    end

  end




end
