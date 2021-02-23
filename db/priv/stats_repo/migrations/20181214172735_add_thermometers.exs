defmodule DB.Repo.Migrations.AddThermometers do
  use Ecto.Migration

  def change do

    create table(:temperature_readings) do
      add :meter_id, :integer
      add :value, :float
      timestamps()
    end

    create unique_index(:temperature_readings, [:meter_id, :inserted_at])
  end
end
