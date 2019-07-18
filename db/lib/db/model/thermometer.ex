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
  end

  def changeset(thermometer, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(thermometer, Enum.into(params, %{}), all_str)
    thermometer
    |> cast(params_, [:name, :address, :ref])
  end

end
