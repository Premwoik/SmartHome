defmodule DB.Repo.Migrations.AddButtons do
  use Ecto.Migration

  def change do
    create table (:rf_buttons) do
      add :name, :string
      add :mode, :string
      add :key_value, :string
      add :port_id, references("ports", on_delete: :nilify_all)
      add :task_id, references("tasks", on_delete: :nilify_all)
      add :action_id, references("actions", on_delete: :nilify_all)
    end
  end
end
