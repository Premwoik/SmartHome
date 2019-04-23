defmodule Ui.Repo.Migrations.CreateDimmers do
  use Ecto.Migration

  def change do
    create table(:dimmers) do
      add :port_id, :integer
      add :fill, :integer
      add :direction, :integer
      add :time, :integer
      add :group, :boolean, default: false, null: false

      timestamps()
    end

  end
end
