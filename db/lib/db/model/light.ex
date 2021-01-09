defmodule DB.Light do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  import DB

  alias DB.{Light, Port, Repo, Dimmer}

  @derive {Poison.Encoder, except: [:__meta__, :dimmer]}
  schema "lights" do
    field(:ref, :integer)
    belongs_to(:port, DB.Port)
    belongs_to(:dimmer, DB.Dimmer)
  end

  def changeset(light, attrs, all_str \\ false) do
    params_ = inc_ref(light, Enum.into(attrs, %{}), all_str)

    light
    |> cast(params_, [:port_id, :dimmer_id, :ref])
  end

  def all() do
    DB.Repo.all(DB.Light)
    |> DB.Repo.preload(:port)
  end

  def update_one(l, params \\ %{}) do
    changeset(l, params)
    |> Repo.update()
  end

  def update(ls, params \\ %{}) do
    Enum.each(ls, fn l -> changeset(l, params) |> Repo.update() end)
    :ok
  end

  def get(ids) when is_list(ids) do
    DB.Repo.all(
      from(l in DB.Light,
        where: l.id in ^ids,
        preload: [
          :port,
          dimmer: [:port]
        ]
      )
    )
  end

  def get(id) do
    [res] = get([id])
    res
  end

  def get_by_port(ids) when is_list(ids) do
    DB.Repo.all(
      from(l in DB.Light,
        where: l.port_id in ^ids,
        preload: [
          :port,
          dimmer: [:port]
        ]
      )
    )
  end

  def get_by_port(id) do
    [res] = get([id])
    res
  end

  def get_by_dimmer(id) do
    from(l in DB.Light, where: l.dimmer_id == ^id, preload: [:port, dimmer: [:port]])
    |> DB.Repo.all()
  end

  def group_by_dimmer(lights) do
    lights
    |> Enum.group_by(fn light -> light.dimmer end)
  end

  def dim_light?(light) do
    light.dimmer != nil
  end

  def get_view_format(id) do
    from(
      l in Light,
      where: l.id == ^id,
      join: c in "page_content_lights",
      on: c.light_id == l.id,
      join: p in Port,
      on: p.id == l.port_id,
      left_join: d in Dimmer,
      on: l.dimmer_id == d.id,
      select: %{
        id: l.id,
        fill: d.fill,
        order: c.order,
        state: p.state,
        name: p.name,
        light: ""
      }
    )
    |> Repo.all()
  end
end
