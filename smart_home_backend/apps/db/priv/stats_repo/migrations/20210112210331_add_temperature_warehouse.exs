defmodule DB.StatsRepo.Migrations.AddTemperatureWarehouse do
  use Ecto.Migration

  def change do
    create table(:temperature_1h) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end
    create table(:temperature_1day) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create table(:temperature_1week) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create table(:temperature_1month) do
      add :meter_id, :integer
      add :date, :naive_datetime
      add :value, :float
    end

    create unique_index(:temperature_1h, [:meter_id, :date])
    create unique_index(:temperature_1day, [:meter_id, :date])
    create unique_index(:temperature_1week, [:meter_id, :date])
    create unique_index(:temperature_1month, [:meter_id, :date])
  end
end
