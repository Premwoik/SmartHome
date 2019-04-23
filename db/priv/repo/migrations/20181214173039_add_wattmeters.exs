defmodule DB.Repo.Migrations.AddWattmeters do
  use Ecto.Migration

  def change do
    create table(:wattmeters) do
      add :name, :string
      add :address, :integer
      add :device_id, references(:devices, on_delete: :delete_all)
    end

    create table(:wattmeter_reads) do
      add :wattmeter_id, references(:wattmeters, on_delete: :delete_all)
      add :value, :integer
      timestamps()
    end
  end
end
