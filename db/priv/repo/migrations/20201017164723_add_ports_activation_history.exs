defmodule DB.Repo.Migrations.AddPortsActivationHistory do
  use Ecto.Migration

  def change do
    create table (:inputs_activations) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :port_id, references("ports", on_delete: :nilify_all)
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:outputs_activations) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :port_id, references("ports", on_delete: :nilify_all)
      add :date, :naive_datetime
      add :value, :integer
    end

    create table(:devices_activations) do
      add :device_id, references("devices", on_delete: :nilify_all)
      add :date, :naive_datetime
      add :value, :integer
    end
  end
end
