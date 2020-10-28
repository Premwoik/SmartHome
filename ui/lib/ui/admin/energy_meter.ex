defmodule Ui.Admin.EnergyMeter do
  use Ecto.Schema
  import Ecto.Changeset


  schema "wattmeters" do
    field :address, :integer
    field :device, :integer
    field :name, :string
    field :ref, :integer

    timestamps()
  end

  @doc false
  def changeset(energy_meter, attrs) do
    energy_meter
    |> cast(attrs, [:device, :name, :address, :ref])
    |> validate_required([:device, :name, :address, :ref])
  end
end
