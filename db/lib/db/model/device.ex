defmodule DB.Device do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Changeset
  import Ecto.Query

  schema "devices" do
  field :name, :string
  field :ip, :string
  field :port, :integer
  field :type, :string
  field :alive, :boolean, default: false
  field :process, :boolean, default: true
  has_many :ports, DB.Port
  end

  def changeset(dimmer, params \\ %{}) do
    dimmer
    |> cast(params, [:name, :ip, :port, :type])
#    |> validate_required([:fill])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end

  def get(id) do
    DB.Repo.get DB.Device, id
  end

  def all() do
    DB.Repo.all DB.Device
  end


end
