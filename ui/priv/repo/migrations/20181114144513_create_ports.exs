defmodule Ui.Repo.Migrations.CreatePorts do
  use Ecto.Migration

  def change do
    create table(:ports) do
      add :name, :string
      add :number, :integer
      add :mode, :string
      add :state, :boolean, default: false, null: false
      add :type, :string
      add :timeout, :integer
      add :device_id, :integer

      timestamps()
    end

  end
end
