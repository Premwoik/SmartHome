defmodule UiWeb.ActionControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.Action

  @create_attrs %{
    active: true,
    end_time: ~T[14:00:00],
    frequency: 42,
    function: "some function",
    params: "some params",
    port_id: 42,
    start_time: ~T[14:00:00]
  }
  @update_attrs %{
    active: false,
    end_time: ~T[15:01:01],
    frequency: 43,
    function: "some updated function",
    params: "some updated params",
    port_id: 43,
    start_time: ~T[15:01:01]
  }
  @invalid_attrs %{
    active: nil,
    end_time: nil,
    frequency: nil,
    function: nil,
    params: nil,
    port_id: nil,
    start_time: nil
  }

  def fixture(:action) do
    {:ok, action} = Admin.create_action(@create_attrs)
    action
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all actions", %{conn: conn} do
      conn = get(conn, Routes.action_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create action" do
    test "renders action when data is valid", %{conn: conn} do
      conn = post(conn, Routes.action_path(conn, :create), action: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.action_path(conn, :show, id))

      assert %{
               "id" => id,
               "active" => true,
               "end_time" => "14:00:00",
               "frequency" => 42,
               "function" => "some function",
               "params" => "some params",
               "port_id" => 42,
               "start_time" => "14:00:00"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.action_path(conn, :create), action: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update action" do
    setup [:create_action]

    test "renders action when data is valid", %{conn: conn, action: %Action{id: id} = action} do
      conn = put(conn, Routes.action_path(conn, :update, action), action: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.action_path(conn, :show, id))

      assert %{
               "id" => id,
               "active" => false,
               "end_time" => "15:01:01",
               "frequency" => 43,
               "function" => "some updated function",
               "params" => "some updated params",
               "port_id" => 43,
               "start_time" => "15:01:01"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, action: action} do
      conn = put(conn, Routes.action_path(conn, :update, action), action: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete action" do
    setup [:create_action]

    test "deletes chosen action", %{conn: conn, action: action} do
      conn = delete(conn, Routes.action_path(conn, :delete, action))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.action_path(conn, :show, action))
      end
    end
  end

  defp create_action(_) do
    action = fixture(:action)
    {:ok, action: action}
  end
end
