defmodule DB.Repo.Migrations.AddTaskTypeParameters do
  use Ecto.Migration

  def change do
    alter table("task_types") do
      add :action, :boolean
      add :device,  :boolean
    end
  end
end
