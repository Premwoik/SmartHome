defmodule DB.Repo.Migrations.AddRefToButtons do
  use Ecto.Migration

  def change do
    alter table(:rf_buttons) do
      add :ref, :integer, default: 1
    end
  end
end
