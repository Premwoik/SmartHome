defmodule DB.Repo.Migrations.AddPageIdToButtons do
  use Ecto.Migration

  def change do
    alter table(:rf_buttons) do
      add :page_id, :integer, default: 1
    end
  end
end
