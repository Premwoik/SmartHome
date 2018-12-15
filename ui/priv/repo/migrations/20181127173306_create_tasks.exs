defmodule Ui.Repo.Migrations.CreateTasks do
  use Ecto.Migration

  def change do
    create table(:tasks) do
      add :type_id, :integer
      add :status, :string
      add :action_id, :integer
      add :device_id, :integer
      add :frequency, :integer
      add :execution_time, :time
      add :limit, :integer
      add :start_date, :date
      add :end_date, :date

      timestamps()
    end

  end
end
