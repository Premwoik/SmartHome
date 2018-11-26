defmodule Ui.Repo.Migrations.CreateKeybs do
  use Ecto.Migration

  def change do
    create table(:keybs) do
      add :name, :string
      add :age, :integer

      timestamps()
    end

  end
end
