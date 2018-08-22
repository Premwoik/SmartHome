defmodule DB.Dimmer do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

#  @primary_key {:port, :id, []}
  @derive {Poison.Encoder, only: [:id, :name, :fill, :lights]}
  schema "dimmers" do
    field :fill, :integer
    field :direction, :integer
    field :time, :integer
    many_to_many :lights, DB.Port, join_through: "dimmers_lights", on_delete: :delete_all
  end

  def changeset(dimmer, params \\ %{}) do
    dimmer
    |> cast(params, [:fill])
    |> validate_required([:fill])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end

end
