defmodule UiWeb.DimmerController do
  use UiWeb, :controller

  alias Ui.DimmerAdmin
  alias Ui.LightAdmin
  alias Core.Controllers.{LightController, DimmerController}
  alias DB.{Dimmer, Light, Port}

  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

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
    #TODO remove below mock
    dimmer_params2 = Map.put(dimmer_params, "time", Map.get(dimmer_params, "full_time"))
    with {:ok, %Dimmer{} = dimmer} <- DimmerAdmin.update_dimmer(dimmer, dimmer_params2) do
      render(conn, "show.json", dimmer: dimmer)
    end
  end

  def delete(conn, %{"id" => id}) do
    dimmer = DimmerAdmin.get_dimmer!(id)

    with {:ok, %Dimmer{}} <- DimmerAdmin.delete_dimmer(dimmer) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => id} = o) do
    DimmerAdmin.get_dimmer!(id)
    |> List.wrap()
    |> DimmerController.turn_on()
    |> case do
      :ok ->
        DashHelper.broadcast_update_from(o, [id], "dimmer")

      _ ->
        :err
    end

    DashHelper.broadcast_update_from(o, [id], "dimmer")

    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dim)
  end

  def set_off(conn, %{"id" => id} = o) do
    DimmerAdmin.get_dimmer!(id)
    |> List.wrap()
    |> DimmerController.turn_off()
    |> case do
      :ok ->
        DashHelper.broadcast_update_from(o, [id], "dimmer")

      _ ->
        :err
    end

    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dim)
  end

  def set_brightness(conn, %{"id" => id, "fill" => fill} = o) do
    fill_ =
      cond do
        fill > 100 -> 100
        fill < 5 -> 5
        true -> fill
      end

    DimmerAdmin.get_dimmer!(id)
    |> DimmerController.set_brightness(fill_)
    |> case do
      :ok ->
        #           DashHelper.broadcast_update_from(o, [id], "dimmer")
        :ok

      _ ->
        :err
    end

    dim = DimmerAdmin.get_dimmer!(id)
    render(conn, "show.json", dimmer: dim)
  end

  def set_light_on(conn, %{"id" => id} = o) do
    l = LightAdmin.get_light!(id)

    LightController.turn_on([l])
    |> case do
      :ok ->
        DashHelper.broadcast_update_from(o, [id], "dimmer")

      _ ->
        :err
    end

    dim = DimmerAdmin.get_dimmer!(l.dimmer_id)
    render(conn, "show.json", dimmer: dim)
  end

  def set_light_off(conn, %{"id" => id} = o) do
    l = LightAdmin.get_light!(id)

    LightController.turn_off([l])
    |> case do
      :ok ->
        DashHelper.broadcast_update_from(o, [id], "dimmer")

      _ ->
        :err
    end

    DashHelper.broadcast_update_from(o, [id], "dimmer")

    dim = DimmerAdmin.get_dimmer!(l.dimmer_id)
    render(conn, "show.json", dimmer: dim)
  end

end
