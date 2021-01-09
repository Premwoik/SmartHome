defmodule DB.Port do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  import DB

  alias DB.{Repo, Port, Device}

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

  @derive {Poison.Encoder, only: [:name, :state, :id]}
  schema "ports" do
    field(:name, :string)
    field(:number, :integer)
    field(:mode, :string)
    field(:state, :boolean)
    field(:pwm_fill, :integer)
    field(:inverted_logic, :boolean)
    field(:type, :string)
    field(:timeout, :integer)
    field(:ref, :integer)
    belongs_to(:device, DB.Device)
    has_one(:light, DB.Light)
    has_one(:dimmer, DB.Dimmer)
    has_one(:sunblind, DB.Sunblind)
  end

  def changeset(port, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(port, Enum.into(params, %{}), all_str)
    port
    |> cast(params_, [:state, :device_id, :number, :name, :type, :mode, :timeout, :pwm_fill, :ref])
  end

  def get_child_struct(port) do
    case port.type do
      "light" -> DB.Light
      "dimmer" -> DB.Dimmer
      "sunblind" -> DB.Sunblind
    end
    |> DB.Repo.get_by(port_id: port.id)
    |> DB.Repo.preload(port: [:device])
  end

  def get_child_id(port) do
    mod =
      case port.type do
        "light" -> DB.Light
        "dimmer" -> DB.Dimmer
        "sunblind" -> DB.Sunblind
      end

    from(c in mod, where: c.port_id == ^port.id, select: c.id)
    |> DB.Repo.one()
  end


  def preload(key \\ :port) do
    Keyword.put([], key, Device.preload())
  end

  def update_low_high(device, high, low) do
    {x, resX} = update_out_of_date(device.id, high, true)
    {y, resY} = update_out_of_date(device.id, low, false)
    {x + y, resX ++ resY}
  end

  def update_out_of_date(_device_id, [], _state), do: {0, []}

  def update_out_of_date(device_id, data, state) do
    res =
      from(p in Port,
        where: p.device_id == ^device_id and p.number in ^data and p.state != ^state
      )
      |> Repo.all()
      |> Enum.map(fn p -> {p.type, get_child_id(p), p.ref + 1} end)

    {x, nil} =
      List.foldr(data, Port, fn num, acc ->
        where(acc, [p], p.device_id == ^device_id and p.number == ^num and p.state != ^state)
      end)
      |> Repo.update_all(set: [state: state], inc: [ref: 1])

    {x, res}
  end

  def get(ids) do
    Repo.all(from(p in Port, where: p.id in ^ids, preload: ^Device.preload()))
  end

  def pulse?(port) do
    port.timeout > 0
  end

  def update_state(ids, state) do
    Repo.update_all(from(p in Port, where: p.id in ^ids),
      set: [
        state: state
      ]
    )
  end

  def update_state2(ports, state) do
    for port <- ports do
      Ecto.Changeset.change(port, state: state)
      |> Repo.update()
    end
  end

  def update(ports, args \\ %{}) do
    for port <- ports do
      changeset(port, args)
      |> Repo.update()
    end
  end
end
