defmodule UiWeb.ActionController do
  use UiWeb, :controller

  alias Core.Controllers.ActionController, as: Controller
  alias Ui.ActionAdmin, as: Admin
  alias DB.Action

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
    action = Admin.get_action!(id)
    Core.Actions.reload_actions()

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

  def set(conn, %{"id" => id, "state" => state} = o) do
    with {:ok, action} <- Admin.get_action(id),
         true <- DB.check_ref(o, action),
         {1, nil} <- Controller.set([action], state) do
      action = Admin.get_action!(id)
      render(conn, "show.json", action: action)
    end
  end

  def set_on(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", true))
  end

  def set_off(conn, %{"id" => _} = o) do
    set(conn, Map.put(o, "state", false))
  end

  def update_args(conn, %{"id" => id, "port_ids" => port_ids}) do
    id = String.to_integer(id)

    with :ok <- Admin.update_action_args(id, port_ids) do
      send_resp(conn, :no_content, "")
    end
  end

  def get_args(conn, %{"id" => id}) do
    args = DB.ActionArgument.get(id)
    render(conn, "show_args.json", %{args: args})
  end

  def get_items(conn, %{"id" => id}) do
    items = Admin.get_action_items(id)
    render(conn, "show_items.json", %{items: items})
  end

  def activate_high(conn, %{"id" => id}) do
    id = String.to_integer(id)
    Core.Actions.activate_up([id])
    send_resp(conn, :no_content, "")
  end


  def activate_low(conn, %{"id" => id}) do
    id = String.to_integer(id)
    Core.Actions.activate_down([id])
    send_resp(conn, :no_content, "")
  end

end
