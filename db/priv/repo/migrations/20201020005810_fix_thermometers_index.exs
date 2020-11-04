defmodule DB.Repo.Migrations.FixThermometersIndex do
  use Ecto.Migration

  def change do

    create unique_index(:therm_temp_reads, [:therm_id, :inserted_at])
    drop unique_index(:thermometers, [:therm_id, :inserted_at])
  end
end
