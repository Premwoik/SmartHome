defmodule DB.Data.Meter do
  use Ecto.Schema

  import Ecto.Changeset

  alias DB.Data.Device
  alias DB.Data.Meter
  alias DB.MainRepo

  @type type_t :: :thermometer | :energy_meter

  @type t :: %Meter{name: String.t(), address: String.t(), type: type_t(), device: Device.t()}

  schema "meters" do
    field(:name, :string)
    field(:address, :string)
    field(:type, Ecto.Enum, values: [:thermometer, :energy_meter])

    belongs_to(:device, Device)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :address, :type, :device_id])
  end

  def identify(device_id, address) do
    MainRepo.get_by(Meter, device_id: device_id, address: address)
  end

  @spec insert(map()) :: {:ok, Meter.t()} | {:error, Ecto.Changeset.t()}
  def insert(params) do
    changeset(%Meter{}, params)
    |> MainRepo.insert()
  end
end
