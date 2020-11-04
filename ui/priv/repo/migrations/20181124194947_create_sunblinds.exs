defmodule Ui.Repo.Migrations.CreateSunblinds do
  use Ecto.Migration

  def change do
    create table(:sunblinds) do
      add :position, :integer
      add :type, :string
      add :full_open_time, :integer
      add :direction, :string
      add :state, :string

      timestamps()
    end

  end
end
