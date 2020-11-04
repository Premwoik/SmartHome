defmodule UiWeb.KeybControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.Keyb

  @create_attrs %{
    age: 42,
    name: "some name"
  }
  @update_attrs %{
    age: 43,
    name: "some updated name"
  }
  @invalid_attrs %{age: nil, name: nil}

  def fixture(:keyb) do
    {:ok, keyb} = Admin.create_keyb(@create_attrs)
    keyb
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all keybs", %{conn: conn} do
      conn = get(conn, Routes.keyb_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create keyb" do
    test "renders keyb when data is valid", %{conn: conn} do
      conn = post(conn, Routes.keyb_path(conn, :create), keyb: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.keyb_path(conn, :show, id))

      assert %{
               "id" => id,
               "age" => 42,
               "name" => "some name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.keyb_path(conn, :create), keyb: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update keyb" do
    setup [:create_keyb]

    test "renders keyb when data is valid", %{conn: conn, keyb: %Keyb{id: id} = keyb} do
      conn = put(conn, Routes.keyb_path(conn, :update, keyb), keyb: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.keyb_path(conn, :show, id))

      assert %{
               "id" => id,
               "age" => 43,
               "name" => "some updated name"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, keyb: keyb} do
      conn = put(conn, Routes.keyb_path(conn, :update, keyb), keyb: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete keyb" do
    setup [:create_keyb]

    test "deletes chosen keyb", %{conn: conn, keyb: keyb} do
      conn = delete(conn, Routes.keyb_path(conn, :delete, keyb))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.keyb_path(conn, :show, keyb))
      end
    end
  end

  defp create_keyb(_) do
    keyb = fixture(:keyb)
    {:ok, keyb: keyb}
  end
end
