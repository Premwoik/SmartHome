defmodule DB.Repo.Migrations.AddSunblinds do
  use Ecto.Migration

  def change do
    create table(:sunblinds) do
      add :port_id, references("ports", on_delete: :nilify_all)
      add :open_port_id, references("ports", on_delete: :nilify_all) 
      add :position, :integer
      add :type, :string
      add :full_open_time, :integer
      add :direction, :string
#      add :mode
      add :state, :string
      add :ref, :integer, default: 1
    end
  end
end
