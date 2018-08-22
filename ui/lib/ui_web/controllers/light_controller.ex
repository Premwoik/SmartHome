defmodule UiWeb.LightController do
  use UiWeb, :controller
  @moduledoc false

  def index(conn, _params) do
    lights = DB.Dao.get_lights()
    json conn |> put_status(:ok), lights
  end

  def update(conn, %{"id" => id} = params) do
    light = DB.Repo.get(DB.Port, id)
    if light do
      perform_update(conn, light, params)
    else
      json conn |> put_status(:not_found),
           %{errors: ["invalid light"]}
    end
  end

  defp perform_update(conn, light, params) do
    changeset = DB.Port.changeset(light, params)
    case DB.Repo.update(changeset) do
      {:ok, light} ->
        json conn |> put_status(:ok), light
      {:error, _result} ->
        json conn |> put_status(:bad_request),
             %{errors: ["unable to update light"]}
    end
  end
end
