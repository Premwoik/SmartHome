defmodule DB.Action do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  alias DB.{Repo, Action, Port}

  @derive {Poison.Encoder, only: [:id, :function, :active, :params, :port]}
  schema "actions" do
    field(:name, :string)
    field(:function, :string)
    field(:active, :boolean)
    field(:params, :string)
    field(:frequency, :integer)
    field(:start_time, :time)
    field(:end_time, :time)
    belongs_to(:port, DB.Port)
    many_to_many(:args, DB.Port, join_through: "actions_arguments")
  end

  def changeset(action, params \\ %{}) do
    action
    |> cast(params, [
      :name,
      :function,
      :active,
      :params,
      :frequency,
      :start_time,
      :end_time,
      :port_id
    ])
  end

  def get(ids) do
    from(a in Action, where: a.id in ^ids)
    |> Repo.all()
    |> Repo.preload(:port)
  end

  def get_view_format(id) do
    from(
      a in Action,
      where: a.id == ^id,
      join: c in "page_content_actions",
      on: c.action_id == a.id,
      select: %{
        id: a.id,
        name: "TODO",
        order: c.order,
        function: a.function,
        state: a.active,
        action: ""
      }
    )
    |> Repo.all()
  end

  def get_by_activator(device_id, numbs) do
    Repo.all(
      from(a in Action,
        join: p in Port,
        on: a.port_id == p.id,
        where: p.device_id == ^device_id and p.number in ^numbs,
        select: a.id
      )
    )
  end

  def get_args_ids(action_id) when is_integer(action_id) do
    from(a in "actions_arguments", where: a.action_id == ^action_id, select: a.port_id)
    |> Repo.all()
  end

  def get_args_ids(action) do
    get_args_ids(action.id)
  end

  def all_active() do
    Repo.all(from(a in Action, where: a.active == true))
  end

  def change_state(ids, state) do
    IO.inspect(ids)

    from(a in Action, where: a.id in ^ids)
    |> Repo.update_all(
      set: [
        active: state
      ]
    )
  end
end
