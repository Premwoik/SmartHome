defmodule UiWeb.SunblindControllerTest do
  use UiWeb.ConnCase

  alias Ui.AdminSunblind
  alias Ui.AdminSunblind.Sunblind

  @create_attrs %{
    direction: "some direction",
    full_open_time: 42,
    position: 42,
    state: "some state",
    type: "some type"
  }
  @update_attrs %{
    direction: "some updated direction",
    full_open_time: 43,
    position: 43,
    state: "some updated state",
    type: "some updated type"
  }
  @invalid_attrs %{direction: nil, full_open_time: nil, position: nil, state: nil, type: nil}

  def fixture(:sunblind) do
    {:ok, sunblind} = AdminSunblind.create_sunblind(@create_attrs)
    sunblind
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all sunblinds", %{conn: conn} do
      conn = get(conn, Routes.sunblind_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create sunblind" do
    test "renders sunblind when data is valid", %{conn: conn} do
      conn = post(conn, Routes.sunblind_path(conn, :create), sunblind: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.sunblind_path(conn, :show, id))

      assert %{
               "id" => id,
               "direction" => "some direction",
               "full_open_time" => 42,
               "position" => 42,
               "state" => "some state",
               "type" => "some type"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.sunblind_path(conn, :create), sunblind: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update sunblind" do
    setup [:create_sunblind]

    test "renders sunblind when data is valid", %{
      conn: conn,
      sunblind: %Sunblind{id: id} = sunblind
    } do
      conn = put(conn, Routes.sunblind_path(conn, :update, sunblind), sunblind: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.sunblind_path(conn, :show, id))

      assert %{
               "id" => id,
               "direction" => "some updated direction",
               "full_open_time" => 43,
               "position" => 43,
               "state" => "some updated state",
               "type" => "some updated type"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, sunblind: sunblind} do
      conn = put(conn, Routes.sunblind_path(conn, :update, sunblind), sunblind: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete sunblind" do
    setup [:create_sunblind]

    test "deletes chosen sunblind", %{conn: conn, sunblind: sunblind} do
      conn = delete(conn, Routes.sunblind_path(conn, :delete, sunblind))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.sunblind_path(conn, :show, sunblind))
      end
    end
  end

  defp create_sunblind(_) do
    sunblind = fixture(:sunblind)
    {:ok, sunblind: sunblind}
  end
end
