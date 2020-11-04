defmodule Ui.Repo.Migrations.CreateDashboards do
  use Ecto.Migration

  def change do
    create table(:dashboards) do
      add :name, :string
      add :title, :string
      add :description, :string
      add :number, :integer

      timestamps()
    end

  end
end
