defmodule DB.Repo.Migrations.AddTasks do
  use Ecto.Migration

  def change do

    create table(:task_types) do
      add :name, :string
      add :module, :string
    end

    create table(:tasks) do
      add :type_id, references("task_types", on_delete: :nilify_all)
      add :status, :string
      add :action_id, references("actions", on_delete: :nilify_all)
      add :device_id, references("devices", on_delete: :nilify_all)
      add :frequency, :integer
      add :execution_time, :time
      add :limit, :integer
      add :start_date, :naive_datetime
      add :end_date, :naive_datetime
    end
  end
end
