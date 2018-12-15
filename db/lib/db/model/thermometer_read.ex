defmodule DB.Thermometer.Read do
  @moduledoc false
  use Ecto.Schema

  schema "thermometer_reads" do
    belongs_to :therm, DB.Thermometer
    field :value, :integer
    timestamps()
  end
end
