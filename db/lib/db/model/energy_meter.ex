defmodule DB.EnergyMeter do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import DB

  schema "wattmeters" do
    belongs_to(:device, DB.Device)
    field(:name, :string)
    field(:address, :integer)
    field(:ref, :integer)
    has_many(:readings, DB.EnergyMeter.Read, foreign_key: :wattmeter_id)
  end

  def changeset(wattmeter, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(wattmeter, Enum.into(params, %{}), all_str)

    wattmeter
    |> cast(params_, [:name, :address, :ref])
  end
end
