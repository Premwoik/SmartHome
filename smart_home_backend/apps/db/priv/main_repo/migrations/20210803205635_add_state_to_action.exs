defmodule DB.MainRepo.Migrations.AddStateToAction do
  use Ecto.Migration

  def change do

    alter table("actions") do
      add(:state, :boolean, default: true)
      add(:module, :string)
    end

  end
end
