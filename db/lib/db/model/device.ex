defmodule DB.Device do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Device, Repo, DeviceType}

  schema "devices" do
    field(:name, :string)
    field(:ip, :string)
    field(:port, :integer)
    belongs_to(:type, DB.DeviceType)
    field(:alive, :boolean, default: false)
    has_many(:ports, DB.Port)
  end

  def changeset(device, params \\ %{}) do
    device
    |> cast(params, [:name, :ip, :port, :type_id, :alive])
  end

  def preload, do: [device: :type]

  def get(id) do
    Repo.get(Device, id)
    |> Repo.preload(:type)
  end

  def all() do
    Repo.all(Device)
    |> Repo.preload(:type)
  end
end
