defmodule UiWeb.DimmerController do
  @moduledoc false

  use UiWeb, :controller
  @device Application.get_env(:core, :devices_module)

  def index(conn, _params) do
    dims = DB.Dao.get_dimmers()
    json conn
         |> put_status(:ok), dims
  end


  def set_fill(conn, %{"id" => id, "fill" => fill}) do
    @device.set_dim_fill(id, fill)
    json conn
      |> put_status(:ok), ""
  end

  def toggle(conn, %{"id" => id}) do
    dim = DB.Dao.get_dimmer_part(id)
    lights = (dim |> DB.Repo.preload [:lights]).lights
    state = if dim.fill > 0, do: false, else: true
    @device.set_dim_lights(lights, state)
    json conn
      |> put_status(:ok), ""
  end

  def toggle_light(conn, %{"id" => id, "lightId" => lightId}) do
    light = DB.Dao.get_port(lightId)
    @device.set_dim_lights [light], !light.state
    json conn
      |> put_status(:ok), ""
  end

end
