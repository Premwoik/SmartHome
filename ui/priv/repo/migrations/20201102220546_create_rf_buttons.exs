defmodule Ui.Repo.Migrations.CreateRfButtons do
  use Ecto.Migration

  def change do
    create table(:rf_buttons) do
      add :port, :string
      add :action, :string
      add :task, :string
      add :name, :string
      add :mode, :string
      add :key_value, :string

      timestamps()
    end

  end
end
