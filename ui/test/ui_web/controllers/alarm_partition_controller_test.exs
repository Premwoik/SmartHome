defmodule UiWeb.AlarmPartitionControllerTest do
  use UiWeb.ConnCase

  alias Ui.Admin
  alias Ui.Admin.AlarmPartition

  @create_attrs %{
    device_id: 42,
    name: "some name",
    number: 42,
    status: 42
  }
  @update_attrs %{
    device_id: 43,
    name: "some updated name",
    number: 43,
    status: 43
  }
  @invalid_attrs %{device_id: nil, name: nil, number: nil, status: nil}

  def fixture(:alarm_partition) do
    {:ok, alarm_partition} = Admin.create_alarm_partition(@create_attrs)
    alarm_partition
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all alarm_partitions", %{conn: conn} do
      conn = get(conn, Routes.alarm_partition_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create alarm_partition" do
    test "renders alarm_partition when data is valid", %{conn: conn} do
      conn =
        post(conn, Routes.alarm_partition_path(conn, :create), alarm_partition: @create_attrs)

      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.alarm_partition_path(conn, :show, id))

      assert %{
               "id" => id,
               "device_id" => 42,
               "name" => "some name",
               "number" => 42,
               "status" => 42
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn =
        post(conn, Routes.alarm_partition_path(conn, :create), alarm_partition: @invalid_attrs)

      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update alarm_partition" do
    setup [:create_alarm_partition]

    test "renders alarm_partition when data is valid", %{
      conn: conn,
      alarm_partition: %AlarmPartition{id: id} = alarm_partition
    } do
      conn =
        put(conn, Routes.alarm_partition_path(conn, :update, alarm_partition),
          alarm_partition: @update_attrs
        )

      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.alarm_partition_path(conn, :show, id))

      assert %{
               "id" => id,
               "device_id" => 43,
               "name" => "some updated name",
               "number" => 43,
               "status" => 43
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, alarm_partition: alarm_partition} do
      conn =
        put(conn, Routes.alarm_partition_path(conn, :update, alarm_partition),
          alarm_partition: @invalid_attrs
        )

      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete alarm_partition" do
    setup [:create_alarm_partition]

    test "deletes chosen alarm_partition", %{conn: conn, alarm_partition: alarm_partition} do
      conn = delete(conn, Routes.alarm_partition_path(conn, :delete, alarm_partition))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.alarm_partition_path(conn, :show, alarm_partition))
      end
    end
  end

  defp create_alarm_partition(_) do
    alarm_partition = fixture(:alarm_partition)
    {:ok, alarm_partition: alarm_partition}
  end
end
