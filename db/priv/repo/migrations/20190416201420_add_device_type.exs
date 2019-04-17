defmodule DB.Repo.Migrations.AddDeviceType do
  use Ecto.Migration

  def change do
    create table(:device_types) do
      add :name, :string
      add :module, :string
    end

    alter table(:devices) do
      #remove :type
      add :type_id, references("device_types", on_delete: :nilify_all)
    end
  end
end
