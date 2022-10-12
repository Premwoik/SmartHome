defmodule DB.Repo.Migrations.AddDeviceJournal do
  use Ecto.Migration

  def change do
    create table(:device_journals) do
      add(:device_id, :integer)
      add(:name, :string)
      add(:type, :string)
      add(:info, :string)
      add(:arguments, :string)
      add(:result, :string)
      timestamps()
    end

    create(unique_index(:device_journals, [:device_id, :inserted_at]))
  end
end
