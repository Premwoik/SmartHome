defmodule DB.Repo.Migrations.AddDeviceJournal do
  use Ecto.Migration

  def change do
    create table(:device_journals) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :type, :string
      add :name, :string
      add :info, :string
      timestamps()
    end
  end
end
