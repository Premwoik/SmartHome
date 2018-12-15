defmodule DB.Repo.Migrations.AddThermometers do
  use Ecto.Migration

  def change do

    create table(:thermometers) do
      add :name, :string
      add :address, :string
      add :device_id, references(:devices, on_delete: :delete_all)
    end

    create table(:therm_temp_reads) do
      add :therm_id, references(:thermometers, on_delete: :delete_all)
      add :value, :integer
      timestamps()
    end

  end
end
