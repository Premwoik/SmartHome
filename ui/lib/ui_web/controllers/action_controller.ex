defmodule UiWeb.ActionController do
  use UiWeb, :controller

  alias DB.Action

  alias Ui.ActionAdmin
  alias Core.Controllers.ActionController
  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  alias UiWeb.Controllers.ErrorHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    actions = ActionAdmin.list_actions()
    render(conn, "index.json", actions: actions)
  end

  def create(conn, %{"action" => action_params}) do
    with {:ok, %Action{} = action} <- ActionAdmin.create_action(action_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.action_path(conn, :show, action))
      |> render("show.json", action: action)
    end
  end

  def show(conn, %{"id" => id}) do
    action = ActionAdmin.get_action!(id)
    render(conn, "show.json", action: action)
  end

  def update(conn, %{"id" => id, "action" => action_params}) do
    action = ActionAdmin.get_action!(id)
    Core.Actions.reload_actions()
    with {:ok, %Action{} = action} <- ActionAdmin.update_action(action, action_params) do
      render(conn, "show.json", action: action)
    end
  end

  def delete(conn, %{"id" => id}) do
    action = ActionAdmin.get_action!(id)

    with {:ok, %Action{}} <- ActionAdmin.delete_action(action) do
      send_resp(conn, :no_content, "")
    end
  end

  def set(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, action} <- ActionAdmin.get_action(id),
         true <- DB.check_ref(o, action),
         {1, nil} <- ActionController.set([action], state)
      do
      action = ActionAdmin.get_action!(id)
      render(conn, "show.json", action: action)
    else
      casual_errors ->
        ErrorHelper.handling_casual_errors(conn, casual_errors)
    end
  end

  def set_on(conn, %{"id" => id} = o) do
    set(conn, Map.put(o, "state", true))
  end

  def set_off(conn, %{"id" => id} = o) do
    set(conn, Map.put(o, "state", false))
  end

  def update_args(conn, %{"id" => id, "port_ids" => port_ids} = r) do
    id_ = String.to_integer(id)
    with :ok <- ActionAdmin.update_action_args(id_, port_ids) do
      send_resp(conn, :no_content, "")
    end
  end

  def get_args(conn, %{"id" => id}) do
    args = DB.ActionArgument.get(id)
    render(conn, "show_args.json", %{args: args})
  end
end
