defmodule DB.Sunblind do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Sunblind}

  schema "sunblinds" do
    belongs_to :port, DB.Port
    field :position, :integer, default: 0
    field :type, :string, default: "only_close" # :only_close | :pulse | :other
    field :full_open_time, :integer, default: 0
#    field :direction, :string, default: "up" # :down | :up
    field :state, :string, default: "open" # :open | :close | :in_move | :position
  end

  def get(ids) do
   from(s in Sunblind, where: s.port_id in ^ids, preload: [:port])
   |> Repo.all()
  end

  def all() do
    Repo.all(Sunblind)
    |> Repo.preload(:port)
  end

#  def get_by_port(ids) do
#
#  end

  def update_state([id|_] = ids, state) when is_integer id do
    from(s in Sunblind, where: s.port_id in ^ids)
    |> Repo.update_all(set: [state: state])
  end
  def update_state(sunblinds, state) when is_list sunblinds do
    ids = Enum.map(sunblinds, fn x -> x.id end)
    from(s in Sunblind, where: s.id in ^ids)
    |> Repo.update_all(set: [state: state])
  end
  def update_state(sunblind, state) do
    Ecto.Changeset.change(sunblind, state: state)
    |> Repo.update()
  end


end
