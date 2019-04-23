defmodule DB.Sunblind do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Sunblind, Port}

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "sunblinds" do
    belongs_to(:port, DB.Port)
    belongs_to(:open_port, DB.Port)
    field(:position, :integer, default: 0)
    # :only_close | :pulse | :other
    field(:type, :string, default: "only_close") # only_close | pulse | pulse2
    field(:full_open_time, :integer, default: 0)
    # :down | :up
    field(:direction, :string, default: "up")
    # :open | :close | :in_move | :position
    field(:state, :string, default: "open")
  end

  def changeset(action, params \\ %{}) do
    action
    |> cast(params, [:state, :direction, :full_open_time, :type, :position])

    #    |> validate_required([:active])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end

  def valid_state?(state) do
    ["open", "close", "in_move", "position"]
    |> Enum.any?(fn s -> s == state end)
  end

  @deprecated "use get_by_port/1 instead"
  def by_port(ids) when is_list(ids) do
    from(s in Sunblind, where: s.port_id in ^ids, preload: [:port])
    |> Repo.all()
  end

  def get_by_port(ids) do
    by_port(ids)
  end

  def get(ids) when is_list(ids) do
    from(s in Sunblind, where: s.id in ^ids, preload: [:port])
    |> Repo.all()
  end

  def get(id) do
    Repo.get(Sunblind, id)
    |> Repo.preload(port: [device: :type])
  end

  def get_type(type) do
    from(s in Sunblind, where: s.type == ^type, preload: [:port])
    |> Repo.all()
  end

  def all() do
    Repo.all(Sunblind)
    |> Repo.preload(:port)
  end

  def update(sunblind, changes \\ %{}) do
    Ecto.Changeset.change(sunblind, changes)
    |> Repo.update!()
  end

  def update_state([id | _] = ids, state) when is_integer(id) do
    from(s in Sunblind, where: s.port_id in ^ids)
    |> Repo.update_all(
      set: [
        state: state
      ]
    )
  end

  def update_state(sunblinds, state) when is_list(sunblinds) do
    ids = Enum.map(sunblinds, fn x -> x.id end)

    from(s in Sunblind, where: s.id in ^ids)
    |> Repo.update_all(
      set: [
        state: state
      ]
    )
  end

  def update_state(sunblind, state) do
    if valid_state?(state) do
      Ecto.Changeset.change(sunblind, state: state)
      |> Repo.update()
    else
      {:error, "incorrect state"}
    end
  end
end
