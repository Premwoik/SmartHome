defmodule DB.StatsRepo.Migrations.AddEnergyWarehouse do
  use Ecto.Migration

  def change do
    create table(:energy_1h) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end
    create table(:energy_1day) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create table(:energy_1week) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create table(:energy_1month) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create unique_index(:energy_1h, [:meter_id, :date])
    create unique_index(:energy_1day, [:meter_id, :date])
    create unique_index(:energy_1week, [:meter_id, :date])
    create unique_index(:energy_1month, [:meter_id, :date])
  end
end
