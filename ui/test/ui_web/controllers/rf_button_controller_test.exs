defmodule UiWeb.RfButtonControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.RfButton

  @create_attrs %{
    action: "some action",
    key_value: "some key_value",
    mode: "some mode",
    name: "some name",
    port: "some port",
    task: "some task"
  }
  @update_attrs %{
    action: "some updated action",
    key_value: "some updated key_value",
    mode: "some updated mode",
    name: "some updated name",
    port: "some updated port",
    task: "some updated task"
  }
  @invalid_attrs %{action: nil, key_value: nil, mode: nil, name: nil, port: nil, task: nil}

  def fixture(:rf_button) do
    {:ok, rf_button} = Admin.create_rf_button(@create_attrs)
    rf_button
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all rf_buttons", %{conn: conn} do
      conn = get(conn, Routes.rf_button_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create rf_button" do
    test "renders rf_button when data is valid", %{conn: conn} do
      conn = post(conn, Routes.rf_button_path(conn, :create), rf_button: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.rf_button_path(conn, :show, id))

      assert %{
               "id" => id,
               "action" => "some action",
               "key_value" => "some key_value",
               "mode" => "some mode",
               "name" => "some name",
               "port" => "some port",
               "task" => "some task"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.rf_button_path(conn, :create), rf_button: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update rf_button" do
    setup [:create_rf_button]

    test "renders rf_button when data is valid", %{
      conn: conn,
      rf_button: %RfButton{id: id} = rf_button
    } do
      conn = put(conn, Routes.rf_button_path(conn, :update, rf_button), rf_button: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.rf_button_path(conn, :show, id))

      assert %{
               "id" => id,
               "action" => "some updated action",
               "key_value" => "some updated key_value",
               "mode" => "some updated mode",
               "name" => "some updated name",
               "port" => "some updated port",
               "task" => "some updated task"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, rf_button: rf_button} do
      conn = put(conn, Routes.rf_button_path(conn, :update, rf_button), rf_button: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete rf_button" do
    setup [:create_rf_button]

    test "deletes chosen rf_button", %{conn: conn, rf_button: rf_button} do
      conn = delete(conn, Routes.rf_button_path(conn, :delete, rf_button))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.rf_button_path(conn, :show, rf_button))
      end
    end
  end

  defp create_rf_button(_) do
    rf_button = fixture(:rf_button)
    {:ok, rf_button: rf_button}
  end
end
