defmodule UiWeb.EnergyMeterControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.EnergyMeter

  @create_attrs %{
    address: 42,
    device: 42,
    name: "some name",
    ref: 42
  }
  @update_attrs %{
    address: 43,
    device: 43,
    name: "some updated name",
    ref: 43
  }
  @invalid_attrs %{address: nil, device: nil, name: nil, ref: nil}

  def fixture(:energy_meter) do
    {:ok, energy_meter} = Admin.create_energy_meter(@create_attrs)
    energy_meter
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all wattmeters", %{conn: conn} do
      conn = get(conn, Routes.energy_meter_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create energy_meter" do
    test "renders energy_meter when data is valid", %{conn: conn} do
      conn = post(conn, Routes.energy_meter_path(conn, :create), energy_meter: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.energy_meter_path(conn, :show, id))

      assert %{
               "id" => id,
               "address" => 42,
               "device" => 42,
               "name" => "some name",
               "ref" => 42
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.energy_meter_path(conn, :create), energy_meter: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update energy_meter" do
    setup [:create_energy_meter]

    test "renders energy_meter when data is valid", %{
      conn: conn,
      energy_meter: %EnergyMeter{id: id} = energy_meter
    } do
      conn =
        put(conn, Routes.energy_meter_path(conn, :update, energy_meter),
          energy_meter: @update_attrs
        )

      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.energy_meter_path(conn, :show, id))

      assert %{
               "id" => id,
               "address" => 43,
               "device" => 43,
               "name" => "some updated name",
               "ref" => 43
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, energy_meter: energy_meter} do
      conn =
        put(conn, Routes.energy_meter_path(conn, :update, energy_meter),
          energy_meter: @invalid_attrs
        )

      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete energy_meter" do
    setup [:create_energy_meter]

    test "deletes chosen energy_meter", %{conn: conn, energy_meter: energy_meter} do
      conn = delete(conn, Routes.energy_meter_path(conn, :delete, energy_meter))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.energy_meter_path(conn, :show, energy_meter))
      end
    end
  end

  defp create_energy_meter(_) do
    energy_meter = fixture(:energy_meter)
    {:ok, energy_meter: energy_meter}
  end
end
