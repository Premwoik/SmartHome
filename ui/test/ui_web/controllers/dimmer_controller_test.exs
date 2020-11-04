defmodule UiWeb.DimmerControllerTest do
  use UiWeb.ConnCase

  alias Ui.DimmerAdmin
  alias Ui.DimmerAdmin.Dimmer

  @create_attrs %{
    direction: 42,
    fill: 42,
    group: true,
    port_id: 42,
    time: 42
  }
  @update_attrs %{
    direction: 43,
    fill: 43,
    group: false,
    port_id: 43,
    time: 43
  }
  @invalid_attrs %{direction: nil, fill: nil, group: nil, port_id: nil, time: nil}

  def fixture(:dimmer) do
    {:ok, dimmer} = DimmerAdmin.create_dimmer(@create_attrs)
    dimmer
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all dimmers", %{conn: conn} do
      conn = get(conn, Routes.dimmer_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create dimmer" do
    test "renders dimmer when data is valid", %{conn: conn} do
      conn = post(conn, Routes.dimmer_path(conn, :create), dimmer: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.dimmer_path(conn, :show, id))

      assert %{
               "id" => id,
               "direction" => 42,
               "fill" => 42,
               "group" => true,
               "port_id" => 42,
               "time" => 42
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.dimmer_path(conn, :create), dimmer: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update dimmer" do
    setup [:create_dimmer]

    test "renders dimmer when data is valid", %{conn: conn, dimmer: %Dimmer{id: id} = dimmer} do
      conn = put(conn, Routes.dimmer_path(conn, :update, dimmer), dimmer: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.dimmer_path(conn, :show, id))

      assert %{
               "id" => id,
               "direction" => 43,
               "fill" => 43,
               "group" => false,
               "port_id" => 43,
               "time" => 43
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, dimmer: dimmer} do
      conn = put(conn, Routes.dimmer_path(conn, :update, dimmer), dimmer: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete dimmer" do
    setup [:create_dimmer]

    test "deletes chosen dimmer", %{conn: conn, dimmer: dimmer} do
      conn = delete(conn, Routes.dimmer_path(conn, :delete, dimmer))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.dimmer_path(conn, :show, dimmer))
      end
    end
  end

  defp create_dimmer(_) do
    dimmer = fixture(:dimmer)
    {:ok, dimmer: dimmer}
  end
end
