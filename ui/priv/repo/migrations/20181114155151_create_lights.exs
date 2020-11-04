defmodule Ui.Repo.Migrations.CreateLights do
  use Ecto.Migration

  def change do
    create table(:lights) do
      add :name, :string
      add :number, :integer
      add :state, :boolean, default: false, null: false
      add :device_id, :integer
      add :dimmer_id, :integer

      timestamps()
    end

  end
end
