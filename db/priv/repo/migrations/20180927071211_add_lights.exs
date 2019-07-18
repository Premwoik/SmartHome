defmodule DB.Repo.Migrations.AddLights do
  use Ecto.Migration

  def change do
    create table(:lights) do
      add :port_id, references("ports", on_delete: :nilify_all)
      add :dimmer_id, references("dimmers", on_delete: :nilify_all)
      add :ref, :integer, default: 1
    end
  end
end
