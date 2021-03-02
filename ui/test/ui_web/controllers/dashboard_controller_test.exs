defmodule UiWeb.DashboardControllerTest do
  use UiWeb.ConnCase

  alias Ui.DashboardAdmin
  alias Ui.DashboardAdmin.Dashboard

  @create_attrs %{
    description: "some description",
    name: "some name",
    number: 42,
    title: "some title"
  }
  @update_attrs %{
    description: "some updated description",
    name: "some updated name",
    number: 43,
    title: "some updated title"
  }
  @invalid_attrs %{description: nil, name: nil, number: nil, title: nil}

  def fixture(:dashboard) do
    {:ok, dashboard} = DashboardAdmin.create_dashboard(@create_attrs)
    dashboard
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all dashboards", %{conn: conn} do
      conn = get(conn, Routes.dashboard_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create dashboard" do
    test "renders dashboard when data is valid", %{conn: conn} do
      conn = post(conn, Routes.dashboard_path(conn, :create), dashboard: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.dashboard_path(conn, :show, id))

      assert %{
               "id" => id,
               "description" => "some description",
               "name" => "some name",
               "number" => 42,
               "title" => "some title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.dashboard_path(conn, :create), dashboard: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update dashboard" do
    setup [:create_dashboard]

    test "renders dashboard when data is valid", %{
      conn: conn,
      dashboard: %Dashboard{id: id} = dashboard
    } do
      conn = put(conn, Routes.dashboard_path(conn, :update, dashboard), dashboard: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.dashboard_path(conn, :show, id))

      assert %{
               "id" => id,
               "description" => "some updated description",
               "name" => "some updated name",
               "number" => 43,
               "title" => "some updated title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, dashboard: dashboard} do
      conn = put(conn, Routes.dashboard_path(conn, :update, dashboard), dashboard: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete dashboard" do
    setup [:create_dashboard]

    test "deletes chosen dashboard", %{conn: conn, dashboard: dashboard} do
      conn = delete(conn, Routes.dashboard_path(conn, :delete, dashboard))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.dashboard_path(conn, :show, dashboard))
      end
    end
  end

  defp create_dashboard(_) do
    dashboard = fixture(:dashboard)
    {:ok, dashboard: dashboard}
  end
end
