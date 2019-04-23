defmodule UiWeb.PortControllerTest do
  use UiWeb.ConnCase

  alias Ui.PortAdmin
  alias Ui.PortAdmin.Port

  @create_attrs %{
    device_id: 42,
    mode: "some mode",
    name: "some name",
    number: 42,
    state: true,
    timeout: 42,
    type: "some type"
  }
  @update_attrs %{
    device_id: 43,
    mode: "some updated mode",
    name: "some updated name",
    number: 43,
    state: false,
    timeout: 43,
    type: "some updated type"
  }
  @invalid_attrs %{device_id: nil, mode: nil, name: nil, number: nil, state: nil, timeout: nil, type: nil}

  def fixture(:port) do
    {:ok, port} = PortAdmin.create_port(@create_attrs)
    port
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all ports", %{conn: conn} do
      conn = get(conn, Routes.port_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create port" do
    test "renders port when data is valid", %{conn: conn} do
      conn = post(conn, Routes.port_path(conn, :create), port: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.port_path(conn, :show, id))

      assert %{
               "id" => id,
               "device_id" => 42,
               "mode" => "some mode",
               "name" => "some name",
               "number" => 42,
               "state" => true,
               "timeout" => 42,
               "type" => "some type"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.port_path(conn, :create), port: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update port" do
    setup [:create_port]

    test "renders port when data is valid", %{conn: conn, port: %Port{id: id} = port} do
      conn = put(conn, Routes.port_path(conn, :update, port), port: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.port_path(conn, :show, id))

      assert %{
               "id" => id,
               "device_id" => 43,
               "mode" => "some updated mode",
               "name" => "some updated name",
               "number" => 43,
               "state" => false,
               "timeout" => 43,
               "type" => "some updated type"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, port: port} do
      conn = put(conn, Routes.port_path(conn, :update, port), port: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete port" do
    setup [:create_port]

    test "deletes chosen port", %{conn: conn, port: port} do
      conn = delete(conn, Routes.port_path(conn, :delete, port))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.port_path(conn, :show, port))
      end
    end
  end

  defp create_port(_) do
    port = fixture(:port)
    {:ok, port: port}
  end
end
