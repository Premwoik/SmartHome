defmodule DB.Port do
  @moduledoc false

  alias DB.{Port, Device, CRUD, Repo}

  @type mode :: :input | :output | :output_pwm | :output_pulse | :virtual
  @type type :: :dimmer | :dimmer2 | :dimmer_rgb | :dimmer_rgbw | :light | :sunblind | :zone
  @type more :: light | dimmer | sunblind | nil
  @type light :: %{dimmer_id: CRUD.foreign()}
  @type dimmer :: %{
          fill: integer,
          direction: integer,
          red: integer,
          green: integer,
          blue: integer,
          white: integer,
          time: integer
        }
  @type sunblind :: %{
          type: atom,
          full_open_time: integer,
          direction: atom,
          state: atom,
          open_port_id: CRUD.foreign()
        }

  @type t :: %Port{
          id: CRUD.id(),
          name: String.t(),
          number: integer,
          device_id: CRUD.foreign(Device),
          mode: mode,
          state: boolean,
          pwm_fill: integer,
          inverted_logic: integer,
          type: type,
          more: more,
          timeout: integer,
          ref: CRUD.ref()
        }

  @derive {Poison.Encoder, only: [:name, :state, :id]}
  use CRUD,
    default: [
      ref: 1,
      type: :unknown,
      inverted_logic: false,
      state: false,
      mode: :output,
      number: 0
    ]

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :device_id,
      :number,
      :mode,
      :state,
      :pwm_fill,
      :inverted_logic,
      :type,
      :more,
      :timeout,
      :ref
    ],
    index: [:name, :number],
    type: :ordered_set,
    autoincrement: true

  def device(%{device_id: {:foreign, Device, id}}) do
    with {:ok, d} <- Device.get(id), do: d
  end

  def more(ports) when is_list(ports), do: Enum.map(ports, &more/1)
  def more(%{more: nil}), do: %{}
  def more(%{more: more}) when is_list(more), do: Map.new(more)
  def more(%{more: more}), do: more
  def more(_), do: nil

  def from_more(%{more: nil}, _), do: nil

  def from_more(%{more: more}, key) when is_map(more) do
    Map.get(more, key, nil)
  end

  def from_more(%{more: more}, key) when is_list(more) do
    Keyword.get(more, key, nil)
  end

  def sunblind_open_port(%{type: :sunblind, more: %{open_port_id: {:foreign, Port, id}}}) do
    get(id)
  end

  def sunblind_open_port(_), do: nil

  def sunblinds() do
    find(is_type?(:sunblind))
  end

  def lights() do
    find(is_type?(:light))
  end

  def dimmers() do
    guards = [:dimmer, :dimmer2, :dimmer_rgb, :dimmer_rgbw] |> list_or()
    find(guards)
  end

  def identify(_, []), do: []

  def identify(device_id, numbers) when is_list(numbers) do
    device_id = {:foreign, Device, device_id}
    match_fn = &:erlang.setelement(4, &1, device_id)
    guards = [orelse_guard(numbers, &{:==, :"$4", &1})]
    find_raw(match_fn, guards)
  end

  def identify(device_id, number) do
    identify(device_id, List.wrap(number)) |> List.first()
  end

  def motion_sensors() do
    find(is_type?(:motion_sensor))
  end

  def any_on?(ports), do: Enum.any?(ports, & &1.state)

  defp orelse_guard([h | t], comp) do
    Enum.reduce(t, comp.(h), fn n, acc -> {:orelse, comp.(n), acc} end)
  end

  defp list_or([h | t], comp \\ &is_type?/1) do
    Enum.reduce(t, comp.(h), fn i, acc -> {:or, comp.(i), acc} end)
  end

  defp is_type?(type), do: {:==, :type, type}

  defmodule Light do
    def __info__(), do: %{attributes: [:dimmer_id]}

    def dimmer(port) do
      Port.from_more(port, :dimmer_id) |> Repo.preload()
    end
  end

  defmodule Sunblind do
    def __info__(), do: %{attributes: [:full_open_time, :state, :type, :open_port_id, :close_port_id]}

    def open_port(port) do
      Port.from_more(port, :open_port) |> Repo.preload()
    end
  end

  defmodule Dimmer do
    def __info__(), do: %{attributes: [:fill, :red, :green, :blue, :white, :direction, :time]}

    def lights(dimmer) do
      Port.lights()
      |> Enum.filter(fn l -> Repo.match?(Port.from_more(l, :dimmer_id), dimmer) end)
    end

    def any_light_on?(dimmer) do
      lights(dimmer)
      |> Enum.any?(& &1.state)
    end

    def preload_lights(dimmer) do
      lights = Port.lights()
        |> Enum.filter(fn 
          %{more: %{dimmer_id: {:foreign, Port, id}}} -> id == dimmer.id
          _otherwise -> false
        end)
      Map.put(dimmer, :more, Map.put(dimmer.more, :lights, lights))
    end

    @spec fill_to_time(map, integer) :: integer
    def fill_to_time(%{more: %{fill: fill, direction: dir, time: time}}, new_fill) when fill != new_fill do
      res = (new_fill - fill) * dir

      cond do
        res > 0 ->
          # good direction
          {get_time(time, res), dir * -1}

        res == 0 ->
          # nothing to do
          {0, dir}

        dir > 0 ->
          # dimmer want to increase brightness, but we want to decrease
          {get_time(time, 200 - fill - new_fill), dir}

        true ->
          # dimmer want to decrease brightness, but we want to increase
          {get_time(time, fill + new_fill), dir}
      end
    end

    def fill_to_time(_, _), do: nil

    @spec get_time(integer, integer) :: integer
    defp get_time(max_time, fill) do
      round(max_time * (fill / 100))
    end
  end

  #  modes:
  #  - input
  #  - output
  #  - output-pwm
  #  - output-pulse
  #  - virtual

  #  types:
  #  - dimmer
  #  - dimmer_rgb
  #  - dimmer_rgbw
  #  - light
  #  - sunblind
  #  - zone
end
