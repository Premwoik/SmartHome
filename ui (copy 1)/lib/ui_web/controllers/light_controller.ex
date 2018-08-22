defmodule LightController do
  use UIWeb, :controller
  @moduledoc false

  def update(conn, %{"id" => id} = params) do
    light = DB.Repo.get(DB.Light, id)
    if light do
      perform_update(conn, light, params)
    else
      json conn |> put_status(:not_found),
           %{errors: ["invalid light"]}
    end
  end

  defp perform_update(conn, light, params) do
    changeset = DB.Light.changeset(light, params)
    case DB.Repo.update(changeset) do
      {:ok, light} ->
        json conn |> put_status(:ok), light
      {:error, _result} ->
        json conn |> put_status(:bad_request),
             %{errors: ["unable to update light"]}
    end
  end
end
