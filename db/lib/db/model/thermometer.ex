defmodule DB.Thermometer do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import DB

  schema "thermometers" do
    belongs_to(:device, DB.Device)
    field(:name, :string)
    field(:address, :string)
    field(:ref, :integer)
    has_many(:readings, DB.Thermometer.Read, foreign_key: :therm_id)
  end

  def changeset(thermometer, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(thermometer, Enum.into(params, %{}), all_str)
    thermometer
    |> cast(params_, [:name, :address, :ref])
  end
#  @doc false
#  def changeset(thermometer, attrs) do
#    thermometer
#    |> cast(attrs, [:name, :address, :device_id, :ref])
#    |> validate_required([:name, :address, :device_id, :ref])
#  end

end
