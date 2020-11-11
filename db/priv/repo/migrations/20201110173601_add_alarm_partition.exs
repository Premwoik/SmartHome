defmodule DB.Repo.Migrations.AddAlarmPartition do
  use Ecto.Migration

  def change do
    create table (:alarm_partitions) do
      add :name, :string
      add :number, :integer
      add :status, :string
      add :device_id, references("devices", on_delete: :nilify_all)
      add :ref, :integer
    end
  end
end
