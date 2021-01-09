defmodule DB.ActionArgument do
  @moduledoc false
  use Ecto.Schema
  #  import Ecto.Changeset
  import Ecto.Query
  alias DB.{Repo, Port, Action, ActionArgument}

  @primary_key false
  schema "actions_arguments" do
    belongs_to(:action, Action)
    belongs_to(:port, Port)
  end

  def insert(action_id, id) do
    %ActionArgument{action_id: action_id, port_id: id}
    |> Repo.insert()
  end

  def get(id) do
    from(
      a in ActionArgument,
      join: p in "ports",
      on: a.port_id == p.id,
      where: a.action_id == ^id,
      select: %{name: p.name, type: p.type, port_id: a.port_id, id: a.action_id}
    )
    |> Repo.all()
  end
end
