defmodule DB.Wattmeter do
  @moduledoc false
  use Ecto.Schema

  schema "wattmeters" do
    belongs_to :device, DB.Device
    field :name, :string
    field :address, :integer
  end
end
