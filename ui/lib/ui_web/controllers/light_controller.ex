defmodule UiWeb.LightController do
  @moduledoc false

  use UiWeb, :controller
  @device Application.get_env(:core, :devices_module)

  alias DB.{Light, Dimmer}
  alias Core.Controllers.LightController

  def index(conn, _params) do
    lights = Light.all()
    IO.inspect(lights)
    json conn
         |> put_status(:ok), lights
  end

  def set_brightness(conn, %{"id" => id, "fill" => fill}) do
    res =
      Dimmer.get(id)
      |> LightController.set_brightness(String.to_integer(fill))
    json conn
         |> put_status(:ok), "#{inspect res}"
  end

  def toggle(conn, %{"id" => id}) do
    res = Light.get([id])
          |> LightController.toggle()

    [data] = Light.get_view_format(id)
    json conn
         |> put_status(:ok), data
  end

  def set_on(conn, %{"id" => id}) do
    res = Light.get([id])
      |> LightController.turn_on()

    [data] = Light.get_view_format id
    json conn, data
  end

  def set_off(conn, %{"id" => id}) do
    res = Light.get([id])
      |> LightController.turn_off()

    [data] = Light.get_view_format id
    json conn, data
  end

end
