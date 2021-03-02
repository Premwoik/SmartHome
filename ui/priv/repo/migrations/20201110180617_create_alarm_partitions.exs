defmodule Ui.Repo.Migrations.CreateAlarmPartitions do
  use Ecto.Migration

  def change do
    create table(:alarm_partitions) do
      add :name, :string
      add :number, :integer
      add :status, :integer
      add :device_id, :integer

      timestamps()
    end

  end
end
