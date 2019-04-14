defmodule UiWeb.ActionController do
  use UiWeb, :controller

  alias Ui.ActionAdmin, as: Admin
  alias DB.Action

  alias Core.Controllers.ActionController
  alias UiWeb.DashboardChannel.Helper, as: DashHelper
  action_fallback(UiWeb.FallbackController)

  def index(conn, _params) do
    actions = Admin.list_actions()
    render(conn, "index.json", actions: actions)
  end

  def create(conn, %{"action" => action_params}) do
    with {:ok, %Action{} = action} <- Admin.create_action(action_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.action_path(conn, :show, action))
      |> render("show.json", action: action)
    end
  end

  def show(conn, %{"id" => id}) do
    action = Admin.get_action!(id)
    render(conn, "show.json", action: action)
  end

  def update(conn, %{"id" => id, "action" => action_params}) do
    IO.inspect(id)
    action = Admin.get_action!(id)

    with {:ok, %Action{} = action} <- Admin.update_action(action, action_params) do
      render(conn, "show.json", action: action)
    end
  end

  def delete(conn, %{"id" => id}) do
    action = Admin.get_action!(id)

    with {:ok, %Action{}} <- Admin.delete_action(action) do
      send_resp(conn, :no_content, "")
    end
  end

  def set_on(conn, %{"id" => id} = o) do
    res =
      DB.Action.get([id])
      |> ActionController.turn_on()

    DashHelper.broadcast_update_from(o, [id], "action")

    action = Admin.get_action!(id)
    render(conn, "show.json", action: action)
  end

  def set_off(conn, %{"id" => id} = o) do
    res =
      DB.Action.get([id])
      |> ActionController.turn_off()

    DashHelper.broadcast_update_from(o, [id], "action")

    action = Admin.get_action!(id)
    render(conn, "show.json", action: action)
  end

  def update_args(conn, r = %{"id" => id, "port_ids" => port_ids}) do
    id_ = String.to_integer(id)
    with :ok <- Admin.update_action_args(id_, port_ids) do
      send_resp(conn, :no_content, "")
    end
  end

  def get_args(conn, %{"id" => id}) do
    args = DB.ActionArgument.get(id)
    render(conn, "show_args.json", %{args: args}) 
  end
end