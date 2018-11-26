defmodule UiWeb.DimmerController do
  use UiWeb, :controller

  alias Ui.DimmerAdmin
  alias Ui.LightAdmin
  alias Core.Controllers.LightController
  alias DB.{Dimmer, Light, Port}

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    dimmers = DimmerAdmin.list_dimmers()
    render(conn, "index.json", dimmers: dimmers)
  end

  def create(conn, %{"dimmer" => dimmer_params}) do
    with {:ok, %Dimmer{} = dimmer} <- DimmerAdmin.create_dimmer(dimmer_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.dimmer_path(conn, :show, dimmer))
      |> render("show.json", dimmer: dimmer)
    end
  end

  def show(conn, %{"id" => id}) do
    dimmer = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dimmer)
  end

  def update(conn, %{"id" => id, "dimmer" => dimmer_params}) do
    dimmer = DimmerAdmin.get_dimmer!(id)

    with {:ok, %Dimmer{} = dimmer} <- DimmerAdmin.update_dimmer(dimmer, dimmer_params) do
      render(conn, "show.json", dimmer: dimmer)
    end
  end

  def delete(conn, %{"id" => id}) do
    dimmer = DimmerAdmin.get_dimmer!(id)

    with {:ok, %Dimmer{}} <- DimmerAdmin.delete_dimmer(dimmer) do
      send_resp(conn, :no_content, "")
    end
  end



  def set_on(conn, %{"id" => id}) do
    res = DB.Light.get_by_dimmer(id)
          |> LightController.turn_on()

    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dash_dimmer: dim)

  end

  def set_off(conn, %{"id" => id}) do
    res = DB.Light.get_by_dimmer(id)
          |> LightController.turn_off()


    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dash_dimmer: dim)

  end

  def set_brightness(conn, %{"id" => id, "fill" => fill}) do
    [dim] = DB.Dimmer.get([id])
    LightController.set_brightness(dim, fill)

    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dash_dimmer: dim)
  end

  def set_light_on(conn, %{"id" => id}) do
    l = LightAdmin.get_light!(id)
    LightController.turn_on([l])

    dim = DimmerAdmin.get_dimmer!(l.dimmer_id)
    render(conn, "show.json", dash_dimmer: dim)
  end

  def set_light_off(conn, %{"id" => id}) do
    l = LightAdmin.get_light!(id)
    LightController.turn_off([l])

    dim = DimmerAdmin.get_dimmer!(l.dimmer_id)
    render(conn, "show.json", dash_dimmer: dim)

  end

end
