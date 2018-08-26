defmodule DB.Action do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset


  @derive {Poison.Encoder, only: [:id, :function, :active, :params, :port]}
  schema "actions" do
    field :function, :string
    field :active, :boolean
    field :params, :string
    belongs_to :port, DB.Port
    many_to_many :args, DB.Port, join_through: "actions_arguments", on_delete: :delete_all
  end

  def changeset(action, params \\ %{}) do
    action
    |> cast(params, [:active])
    |> validate_required([:active])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end

end
