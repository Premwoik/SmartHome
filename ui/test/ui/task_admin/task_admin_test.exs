defmodule Ui.TaskAdminTest do
  use Ui.DataCase

  alias Ui.TaskAdmin

  describe "tasks" do
    alias Ui.TaskAdmin.Task

    @valid_attrs %{action_id: 42, device_id: 42, end_date: ~D[2010-04-17], execution_time: ~T[14:00:00], frequency: 42, limit: 42, start_date: ~D[2010-04-17], status: "some status", type_id: 42}
    @update_attrs %{action_id: 43, device_id: 43, end_date: ~D[2011-05-18], execution_time: ~T[15:01:01], frequency: 43, limit: 43, start_date: ~D[2011-05-18], status: "some updated status", type_id: 43}
    @invalid_attrs %{action_id: nil, device_id: nil, end_date: nil, execution_time: nil, frequency: nil, limit: nil, start_date: nil, status: nil, type_id: nil}

    def task_fixture(attrs \\ %{}) do
      {:ok, task} =
        attrs
        |> Enum.into(@valid_attrs)
        |> TaskAdmin.create_task()

      task
    end

    test "list_tasks/0 returns all tasks" do
      task = task_fixture()
      assert TaskAdmin.list_tasks() == [task]
    end

    test "get_task!/1 returns the task with given id" do
      task = task_fixture()
      assert TaskAdmin.get_task!(task.id) == task
    end

    test "create_task/1 with valid data creates a task" do
      assert {:ok, %Task{} = task} = TaskAdmin.create_task(@valid_attrs)
      assert task.action_id == 42
      assert task.device_id == 42
      assert task.end_date == ~D[2010-04-17]
      assert task.execution_time == ~T[14:00:00]
      assert task.frequency == 42
      assert task.limit == 42
      assert task.start_date == ~D[2010-04-17]
      assert task.status == "some status"
      assert task.type_id == 42
    end

    test "create_task/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = TaskAdmin.create_task(@invalid_attrs)
    end

    test "update_task/2 with valid data updates the task" do
      task = task_fixture()
      assert {:ok, %Task{} = task} = TaskAdmin.update_task(task, @update_attrs)
      assert task.action_id == 43
      assert task.device_id == 43
      assert task.end_date == ~D[2011-05-18]
      assert task.execution_time == ~T[15:01:01]
      assert task.frequency == 43
      assert task.limit == 43
      assert task.start_date == ~D[2011-05-18]
      assert task.status == "some updated status"
      assert task.type_id == 43
    end

    test "update_task/2 with invalid data returns error changeset" do
      task = task_fixture()
      assert {:error, %Ecto.Changeset{}} = TaskAdmin.update_task(task, @invalid_attrs)
      assert task == TaskAdmin.get_task!(task.id)
    end

    test "delete_task/1 deletes the task" do
      task = task_fixture()
      assert {:ok, %Task{}} = TaskAdmin.delete_task(task)
      assert_raise Ecto.NoResultsError, fn -> TaskAdmin.get_task!(task.id) end
    end

    test "change_task/1 returns a task changeset" do
      task = task_fixture()
      assert %Ecto.Changeset{} = TaskAdmin.change_task(task)
    end
  end
end
