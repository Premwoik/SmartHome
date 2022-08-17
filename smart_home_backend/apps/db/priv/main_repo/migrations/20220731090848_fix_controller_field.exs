defmodule DB.MainRepo.Migrations.FixControllerField do
  use Ecto.Migration

  def change do
    rename table("rf_buttons"), :controller, to: :controller_id
  end
end
