defmodule DB.Repo.Migrations.AddWattmeters do
  use Ecto.Migration

  def change do

    create table(:energy_readings) do
      add :meter_id, :integer
      add :value, :float
      timestamps()
    end

    create unique_index(:energy_readings, [:meter_id, :inserted_at])
  end
end
