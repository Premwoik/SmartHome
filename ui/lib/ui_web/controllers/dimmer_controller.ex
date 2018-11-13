defmodule UiWeb.DimmerController do
  use UiWeb, :controller
  @moduledoc false

  alias DB.Dimmer
  alias Core.Controllers.LightController

  def index(conn, _params) do
    lights = Dimmer.all()
    IO.inspect(lights)
    json conn
         |> put_status(:ok), lights
  end

  def toggle(conn, %{"id" => id}) do
    res = DB.Light.get_by_dimmer(id)
          |> LightController.toggle()
    json conn
         |> put_status(:ok), "#{inspect res}"
  end


  def set_on(conn, %{"id" => id}) do
    res = DB.Light.get_by_dimmer(id)
          |> LightController.turn_on()


    [data] = DB.Dimmer.get_view_format(id)
    json conn
         |> put_status(:ok), data
  end

  def set_off(conn, %{"id" => id}) do
    res = DB.Light.get_by_dimmer(id)
          |> LightController.turn_off()


    [data] = DB.Dimmer.get_view_format(id)
    json conn
         |> put_status(:ok), data

  end

  def set_brightness(conn, %{"id" => id, "fill" => fill}) do
    [dim] = DB.Dimmer.get([id])
    LightController.set_brightness(dim, fill)

    [data] = DB.Dimmer.get([id])
    json conn
         |> put_status(:ok), data
  end

  def set_light_on(conn, %{"id" => id}) do
    [l] = ls = DB.Light.get([id])
    LightController.turn_on(ls)

    [data] = DB.Dimmer.get_view_format(l.dimmer_id)

    json conn, data
  end

  def set_light_off(conn, %{"id" => id}) do
    [l] = ls = DB.Light.get([id])
    LightController.turn_off(ls)

    [data] = DB.Dimmer.get_view_format(l.dimmer_id)

    json conn, data
  end


end
