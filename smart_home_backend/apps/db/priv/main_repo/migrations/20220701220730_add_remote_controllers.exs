defmodule DB.MainRepo.Migrations.AddRemoteControllers do
  use Ecto.Migration

  def change do
    create table("remote_controllers") do
      add :name, :string 
      add :cols, :integer
    end

    alter table("rf_buttons") do
      add :controller, references("remote_controllers")
    end

  end
end
