defmodule DB.Dimmer do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  import DB
  alias DB.{Repo, Light, Port, Dimmer}

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "dimmers" do
    belongs_to(:port, DB.Port)
    field(:type, :string, default: "click")
    field(:fill, :integer)
    field(:direction, :integer)
    field(:red, :integer)
    field(:green, :integer)
    field(:blue, :integer)
    field(:time, :integer)
    field(:ref, :integer)
    has_many(:lights, DB.Light)
  end

  def changeset(dimmer, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(dimmer, Enum.into(params, %{}), all_str)
    dimmer
    |> cast(params_, [:red, :green, :blue, :fill, :port_id, :direction, :time, :type, :ref])
  end

  def all() do
    Repo.all(Dimmer)
    |> Repo.preload([:port, lights: [:port]])
  end

  def get(ids) when is_list(ids) do
    Repo.all(from(d in Dimmer, where: d.port_id in ^ids, preload: [:port]))
  end

  def get(id) do
    [res] = get([id])
    res
  end


  def get_by_port(ids) when is_list(ids) do
    DB.Repo.all(
      from(
        d in DB.Dimmer,
        where: d.port_id in ^ids,
        preload: [
          port: [:device]
        ]
      )
    )
  end

  def get_view_format(id) do
    load_lights = fn id ->
      from(
        l in Light,
        join: p in Port,
        on: p.id == l.port_id,
        where: l.dimmer_id == ^id,
        select: %{
          id: l.id,
          state: p.state,
          name: p.name
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
    |> Repo.all()
    |> Enum.map(&Map.put(&1, :lights, load_lights.(&1.id)))
  end

  def any_light_on?(dimmer) do
    Repo.all(
      from(
        l in Light,
        join: p in Port,
        on: p.id == l.port_id,
        where: l.dimmer_id == ^dimmer.id and p.state == true
      )
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

  def update_color(dim, fill, red, green, blue) do
    changeset(dim, fill: fill, red: red, green: green, blue: blue)
    |> Repo.update()
  end

  def update_fill(dim, fill, dir) do
    #dir = if fill == 0, do: 1, else: dim.direction * -1
    changeset(dim, fill: fill, direction: dir)
    |> Repo.update()
  end

  def update(dim, args \\ %{}) do
    changeset(dim, args)
    |> Repo.update()
  end

  @spec fill_to_time(map, integer) :: integer
  def fill_to_time(%{fill: fill, direction: dir, time: time}, new_fill) do
    res = (new_fill - fill) * dir
    #IO.puts("fill_to_time, dimmer.fill: #{inspect fill}, new_fill: #{inspect new_fill}, direction: #{inspect dir}")
    cond do
      res > 0 ->
        #good direction
        #IO.puts ("good direction")
        {get_time(time, res), dir * -1}

      res == 0 ->
        #IO.puts("NOTHING")
        #nothing to do 
        {0, dir}

      dir > 0 ->
        #IO.puts("dim inc, but dec")
        # dimmer want to increase brightness, but we want to decrease
        {get_time(time, 200 - fill - new_fill), dir}

      true ->
        #IO.puts("dim dec, but inc")
        # dimmer want to decrease brightness, but we want to increase
        {get_time(time, fill + new_fill), dir}
    end
  end

  @spec get_time(integer, integer) :: integer
  defp get_time(max_time, fill) do
    round(max_time * (fill / 100))
  end
end
