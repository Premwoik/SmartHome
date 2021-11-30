defmodule DB.Data.Device do
  @doc """
    The device data.
  """
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.Data.Device
  alias DB.MainRepo

  @type type_t :: :SonoffBasic | :Shelly | :Arduino | :SonoffRfBridge | :ShellyRGBW2 | :Satel | :BasementPi
  @type status_t :: :online | :unknown | :offline

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %Device{
          id: integer(),
          name: String.t(),
          port: integer(),
          type: type_t(),
          status: status_t()
        }

  schema "devices" do
    field(:name, :string)
    field(:ip, :string)
    field(:port, :integer)
    # FIXME add real module atoms
    field(:type, Ecto.Enum,
      values: [:SonoffBasic, :Shelly, :Arduino, :SonoffRfBridge, :ShellyRGBW2, :Satel, :BasementPi]
    )

    field(:status, Ecto.Enum, values: [:online, :unknown, :offline], default: :unknown)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :ip, :port, :type])
  end

  def get_by_name(name) do
    case MainRepo.get_by(Device, name: name) do
      nil -> {:error, :not_found}
      device -> {:ok, device}
    end
  end

  def get_by_type(type) do
    from(d in Device, where: d.type == ^type)
    |> MainRepo.all()
  end

  def list_all() do
    {:ok, MainRepo.all(Device)}
  end

  def list_all!() do
    MainRepo.all(Device)
  end

  def find(device_id) do
    case MainRepo.get(Device, device_id) do
      nil -> {:error, :not_found}
      device -> {:ok, device}
    end
  end
end
