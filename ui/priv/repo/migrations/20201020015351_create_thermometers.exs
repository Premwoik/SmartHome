defmodule Ui.Repo.Migrations.CreateThermometers do
  use Ecto.Migration

  def change do
    create table(:thermometers) do
      add :name, :string
      add :address, :string
      add :device_id, :integer
      add :ref, :integer

      timestamps()
    end

  end
end
