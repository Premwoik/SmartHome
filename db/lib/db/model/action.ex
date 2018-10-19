defmodule DB.Action do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  alias DB.{Repo, Action, Port}


  @derive {Poison.Encoder, only: [:id, :function, :active, :params, :port]}
  schema "actions" do
    field :function, :string
    field :active, :boolean
    field :params, :string
    field :frequency, :integer
    field :start_time, :time
    field :end_time, :time
    belongs_to :port, DB.Port
    many_to_many :args, DB.Port, join_through: "actions_arguments"
  end

  def changeset(action, params \\ %{}) do
    action
    |> cast(params, [:active])
    |> validate_required([:active])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end


  def get(ids) do
    :todo
  end

  def get_by_activator(device_id, numbs) do
    Repo.all from a in Action, join: p in Port,
                               on: a.port_id == p.id,
                               where: p.device_id == ^device_id and p.number in ^numbs,
                               select: a.id
  end

  def get_args_ids(action_id) when is_integer(action_id) do
   from(a in "actions_arguments", where: a.action_id == ^action_id, select: a.port_id)
   |> Repo.all()
  end
  def get_args_ids(action) do
    get_args_ids(action.id)
  end

  def all_active() do
    Repo.all from a in Action, where: a.active == true
  end



end
