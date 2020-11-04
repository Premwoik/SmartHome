defmodule UiWeb.ThermometerControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.Thermometer

  @create_attrs %{
    address: "some address",
    device_id: 42,
    name: "some name",
    ref: 42
  }
  @update_attrs %{
    address: "some updated address",
    device_id: 43,
    name: "some updated name",
    ref: 43
  }
  @invalid_attrs %{address: nil, device_id: nil, name: nil, ref: nil}

  def fixture(:thermometer) do
    {:ok, thermometer} = Admin.create_thermometer(@create_attrs)
    thermometer
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all thermometers", %{conn: conn} do
      conn = get(conn, Routes.thermometer_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create thermometer" do
    test "renders thermometer when data is valid", %{conn: conn} do
      conn = post(conn, Routes.thermometer_path(conn, :create), thermometer: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.thermometer_path(conn, :show, id))

      assert %{
               "id" => id,
               "address" => "some address",
               "device_id" => 42,
               "name" => "some name",
               "ref" => 42
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.thermometer_path(conn, :create), thermometer: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update thermometer" do
    setup [:create_thermometer]

    test "renders thermometer when data is valid", %{conn: conn, thermometer: %Thermometer{id: id} = thermometer} do
      conn = put(conn, Routes.thermometer_path(conn, :update, thermometer), thermometer: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.thermometer_path(conn, :show, id))

      assert %{
               "id" => id,
               "address" => "some updated address",
               "device_id" => 43,
               "name" => "some updated name",
               "ref" => 43
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, thermometer: thermometer} do
      conn = put(conn, Routes.thermometer_path(conn, :update, thermometer), thermometer: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete thermometer" do
    setup [:create_thermometer]

    test "deletes chosen thermometer", %{conn: conn, thermometer: thermometer} do
      conn = delete(conn, Routes.thermometer_path(conn, :delete, thermometer))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.thermometer_path(conn, :show, thermometer))
      end
    end
  end

  defp create_thermometer(_) do
    thermometer = fixture(:thermometer)
    {:ok, thermometer: thermometer}
  end
end
