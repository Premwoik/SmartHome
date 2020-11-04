defmodule UiWeb.DimmerController do
  use UiWeb, :controller

  alias Ui.{DimmerAdmin}
  alias Core.Controllers.{LightController, DimmerController}
  alias DB.{Dimmer, Light, Port}
  alias UiWeb.Controllers.ErrorHelper

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
    set(conn, Map.put(o, "fill", 100))
  end

  def set_off(conn, %{"id" => id} = o) do
    set(conn, Map.put(o, "fill", 0))
  end

  def set(conn, %{"id" => id, "fill" => fill, "red" => r, "green" => g, "blue" => b} = o) do
    fill_ =
      cond do
        fill > 100 -> 100
        fill < 0 -> 0
        true -> fill
      end

    with {:ok, dim} <- DimmerAdmin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p fn -> DimmerController.set_rgb_brightness(dim, fill_, r, g, b) end
      do
      dim = DimmerAdmin.get_dimmer!(id)
      render(conn, "show.json", dimmer: dim)
    else
      casual_errors ->
        IO.inspect(casual_errors)
        ErrorHelper.handling_casual_errors(conn, casual_errors)
    end
  end

  def set(conn, %{"id" => id, "fill" => fill} = o) do
    fill_ =
      cond do
        fill > 100 -> 100
        fill < 0 -> 0
        true -> fill
      end

    with {:ok, dim} <- DimmerAdmin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p fn -> DimmerController.set_brightness(dim, fill_) end
      do
      dim = DimmerAdmin.get_dimmer!(id)
      render(conn, "show.json", dimmer: dim)
    else
      casual_errors ->
        IO.inspect(casual_errors)
        ErrorHelper.handling_casual_errors(conn, casual_errors)
    end
  end
  def set(conn, %{"id" => id, "white" => fill} = o) do
    fill_ =
      cond do
        fill > 100 -> 100
        fill < 0 -> 0
        true -> fill
      end

    with {:ok, dim} <- DimmerAdmin.get_dimmer(id),
         true <- DB.check_ref(o, dim),
         :ok <- Benchmark.measure_p fn -> DimmerController.set_white_brightness(dim, fill_) end
      do
      dim = DimmerAdmin.get_dimmer!(id)
      render(conn, "show.json", dimmer: dim)
    else
      casual_errors ->
        IO.inspect(casual_errors)
        ErrorHelper.handling_casual_errors(conn, casual_errors)
    end
  end

end
