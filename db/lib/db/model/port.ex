defmodule DB.Port do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @derive {Poison.Encoder, only: [:name, :state, :id]}
  schema "ports" do
    field :name, :string
    field :number, :integer
    field :mode, :string
    field :state, :boolean
    field :type, :string
    field :timeout, :integer
    belongs_to :device, DB.Device
  end


  def changeset(port, params \\ %{}) do
    port
    |> cast(params, [:state, :number, :name, :type])
    |> validate_required([:state])
#    |> validate_format(:email, ~r/@/)
#    |> validate_inclusion(:age, 18..100)
#    |> unique_constraint(:email)
  end

end
