defmodule DB.Thermometer do
  @moduledoc false
  use Ecto.Schema
  schema "thermometers" do
    belongs_to :device, DB.Device
    field :name, :string
    field :address, :string
  end



end
