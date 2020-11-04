defmodule DB.Repo.Migrations.AddWattmeters do
  use Ecto.Migration

  def change do
    create table(:wattmeters) do
      add :name, :string
      add :address, :integer
      add :device_id, references(:devices, on_delete: :delete_all)
      add :ref, :integer, default: 1
    end

    create table(:wattmeter_reads) do
      add :wattmeter_id, references(:wattmeters, on_delete: :delete_all)
      add :value, :integer
      timestamps()
    end

    create unique_index(:wattmeter_reads, [:wattmeter_id, :inserted_at])
  end
end
