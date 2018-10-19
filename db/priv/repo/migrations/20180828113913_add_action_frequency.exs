defmodule DB.Repo.Migrations.AddActionFrequency do
  use Ecto.Migration

  def change do
    alter table ("actions") do
      add :frequency, :integer, default: 15_000
    end
  end
end
