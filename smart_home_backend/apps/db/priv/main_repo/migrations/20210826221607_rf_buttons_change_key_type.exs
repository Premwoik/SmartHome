defmodule DB.MainRepo.Migrations.RfButtonsChangeKeyType do
  use Ecto.Migration

  def change do
    alter table("rf_buttons") do
      modify(:key_value, :string)
    end

  end
end
