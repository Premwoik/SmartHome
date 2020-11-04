defmodule Ui.Repo.Migrations.CreateWattmeters do
  use Ecto.Migration

  def change do
    create table(:wattmeters) do
      add :device, :integer
      add :name, :string
      add :address, :integer
      add :ref, :integer

      timestamps()
    end

  end
end
