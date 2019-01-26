defmodule DB.Thermometer.Read do
  @moduledoc false
  use Ecto.Schema

  schema "therm_temp_reads" do
    belongs_to :therm, DB.Thermometer
    field :value, :float
    timestamps()
  end
end
