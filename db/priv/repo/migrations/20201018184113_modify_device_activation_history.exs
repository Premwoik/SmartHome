defmodule DB.Repo.Migrations.ModifyDeviceActivationHistory do
  use Ecto.Migration

  def change do
    alter table("devices_activations") do
      add :infos, :string, default: ""
    end
  end
end
