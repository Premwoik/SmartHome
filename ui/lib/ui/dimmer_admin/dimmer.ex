defmodule Ui.DimmerAdmin.Dimmer do
  use Ecto.Schema
  import Ecto.Changeset


  schema "dimmers" do
    field :direction, :integer
    field :fill, :integer
    field :group, :boolean, default: false
    field :port_id, :integer
    field :time, :integer

    timestamps()
  end

  @doc false
  def changeset(dimmer, attrs) do
    dimmer
    |> cast(attrs, [:port_id, :fill, :direction, :time, :group])
    |> validate_required([:port_id, :fill, :direction, :time, :group])
  end
end
