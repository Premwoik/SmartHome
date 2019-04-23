defmodule DB.Repo.Migrations.AddActionTime do
  use Ecto.Migration

  def change do
    alter table("actions") do
      add :start_time, :time # Database type
      add :end_time,  :time  # Elixir type which is handled by the database
    end
  end
end
