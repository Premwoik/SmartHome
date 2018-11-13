defmodule DB.Dimmer do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer}

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "dimmers" do
    belongs_to :port, DB.Port
    field :fill, :integer
    field :direction, :integer
    field :time, :integer
    has_many :lights, DB.Light
    #    many_to_many :lights, DB.Port, join_through: "dimmers_lights", on_delete: :delete_all
  end

  def changeset(dimmer, params \\ %{}) do
    dimmer
    |> cast(params, [:fill])
    |> validate_required([:fill])
    #    |> validate_format(:email, ~r/@/)
    #    |> validate_inclusion(:age, 18..100)
    #    |> unique_constraint(:email)
  end

  def all() do
    Repo.all(Dimmer)
    |> Repo.preload([:port, lights: [:port]])
  end

  def get(ids) when is_list ids do
    Repo.all from d in Dimmer, where: d.port_id in ^ids, preload: [:port]
  end
  def get(id) do
    [res] = get([id])
    res
  end

  def get_view_format(id) do

    load_lights =
      fn id ->
        from(
          l in Light,
          join: p in Port,
          on: p.id == l.port_id,
          where: l.dimmer_id == ^id,
          select: %{
            id: l.id,
            state: p.state,
            name: p.name,
          }
        )
        |> Repo.all()
      end

    from(
      d in Dimmer,
      where: d.id == ^id,
      join: c in "page_content_dimmers",
      on: c.dimmer_id == d.id,
      join: p in Port,
      on: p.id == d.port_id,
      select: %{
        id: d.id,
        fill: d.fill,
        order: c.order,
        name: p.name,
        dimmer: ""
      }
    )
    |> Repo.all
    |> Enum.map(&(Map.put(&1, :lights, load_lights.(&1.id))))
  end




  def any_light_on?(dimmer) do
    Repo.all(
      from l in Light, join: p in Port, on: p.id == l.port_id, where: l.dimmer_id == ^dimmer.id and p.state == true
    )
    |> Kernel.length()
    |> Kernel.>(0)
  end

  def update_fill(dims) do
    Enum.each(
      dims,
      fn dim ->
        Ecto.Changeset.change(dim, fill: round(dim.port.timeout / dim.time * 100), direction: -1)
        |> Repo.update()
      end
    )
  end

  def update_fill(dim, fill) do
    Ecto.Changeset.change(dim, fill: fill, direction: dim.direction * -1)
    |> Repo.update()
  end

  def update_fill(ids, fill, direction) do
    Repo.update_all (from d in Dimmer, where: d.port_id in ^ids),
                    set: [
                      fill: fill,
                      direction: direction
                    ]
  end

  @spec fill_to_time(map, integer) :: integer
  def fill_to_time(dimmer, new_fill) do
    res = (new_fill - dimmer.fill) * dimmer.direction
    cond do
      res > 0 ->
        get_time(dimmer.time, res)
      res == 0 ->
        0
      dimmer.direction > 0 ->
        get_time(dimmer.time, 200 - dimmer.fill - new_fill)
      true ->
        get_time(dimmer.time, dimmer.fill + new_fill)
    end
  end

  @spec get_time(integer, integer) :: integer
  defp get_time(max_time, fill) do
    cond do
      fill > 150 -> max_time * 1.75
      fill > 125 -> max_time * 1.50
      fill > 100 -> max_time * 1.25
      fill > 75 -> max_time
      fill > 50 -> max_time * 0.75
      fill > 25 -> max_time * 0.5
      fill > 0 -> max_time * 0.25
      true -> 0
    end
    |> round
  end


end
