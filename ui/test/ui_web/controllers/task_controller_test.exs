defmodule UiWeb.TaskControllerTest do
  use UiWeb.ConnCase

  alias Ui.TaskAdmin
  alias Ui.TaskAdmin.Task

  @create_attrs %{
    action_id: 42,
    device_id: 42,
    end_date: ~D[2010-04-17],
    execution_time: ~T[14:00:00],
    frequency: 42,
    limit: 42,
    start_date: ~D[2010-04-17],
    status: "some status",
    type_id: 42
  }
  @update_attrs %{
    action_id: 43,
    device_id: 43,
    end_date: ~D[2011-05-18],
    execution_time: ~T[15:01:01],
    frequency: 43,
    limit: 43,
    start_date: ~D[2011-05-18],
    status: "some updated status",
    type_id: 43
  }
  @invalid_attrs %{
    action_id: nil,
    device_id: nil,
    end_date: nil,
    execution_time: nil,
    frequency: nil,
    limit: nil,
    start_date: nil,
    status: nil,
    type_id: nil
  }

  def fixture(:task) do
    {:ok, task} = TaskAdmin.create_task(@create_attrs)
    task
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all tasks", %{conn: conn} do
      conn = get(conn, Routes.task_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create task" do
    test "renders task when data is valid", %{conn: conn} do
      conn = post(conn, Routes.task_path(conn, :create), task: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.task_path(conn, :show, id))

      assert %{
               "id" => id,
               "action_id" => 42,
               "device_id" => 42,
               "end_date" => "2010-04-17",
               "execution_time" => "14:00:00",
               "frequency" => 42,
               "limit" => 42,
               "start_date" => "2010-04-17",
               "status" => "some status",
               "type_id" => 42
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.task_path(conn, :create), task: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update task" do
    setup [:create_task]

    test "renders task when data is valid", %{conn: conn, task: %Task{id: id} = task} do
      conn = put(conn, Routes.task_path(conn, :update, task), task: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.task_path(conn, :show, id))

      assert %{
               "id" => id,
               "action_id" => 43,
               "device_id" => 43,
               "end_date" => "2011-05-18",
               "execution_time" => "15:01:01",
               "frequency" => 43,
               "limit" => 43,
               "start_date" => "2011-05-18",
               "status" => "some updated status",
               "type_id" => 43
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, task: task} do
      conn = put(conn, Routes.task_path(conn, :update, task), task: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete task" do
    setup [:create_task]

    test "deletes chosen task", %{conn: conn, task: task} do
      conn = delete(conn, Routes.task_path(conn, :delete, task))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.task_path(conn, :show, task))
      end
    end
  end

  defp create_task(_) do
    task = fixture(:task)
    {:ok, task: task}
  end
end
