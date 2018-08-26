defmodule UiWeb.ActionController do
  @moduledoc false
  use UiWeb, :controller

  def index(conn, _params) do
    actions = DB.Dao.get_actions()
    json conn |> put_status(:ok), actions
  end

  def toggle(conn, %{"id" => id}) do
    action = DB.Dao.find_action(id)
    if action do
      DB.Dao.set_action_state(action, !action.active)
      Alarm.Actions.reload_actions()
    end
    json conn
         |> put_status(:ok), ""
  end

  def update(conn, %{"id" => id} = params) do
    action = DB.Repo.get(DB.Action, id)
    if action do
      perform_update(conn, action, params)
    else
      json conn |> put_status(:not_found),
           %{errors: ["invalid action"]}
    end
  end

  defp perform_update(conn, action, params) do
    changeset = DB.Action.changeset(action, params)
    case DB.Repo.update(changeset) do
      {:ok, action} ->
        json conn |> put_status(:ok), action
      {:error, _result} ->
        json conn |> put_status(:bad_request),
             %{errors: ["unable to update action"]}
    end
  end

end
