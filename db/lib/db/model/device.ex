defmodule DB.Device do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Changeset
  import Ecto.Query

  import DB
  alias DB.{Device, Repo, DeviceType}

  schema "devices" do
    field(:name, :string)
    field(:ip, :string)
    field(:port, :integer)
    field(:password, :string, virtual: true)
    belongs_to(:type, DB.DeviceType)
    field(:alive, :boolean, default: false)
    field(:ref, :integer)
    has_many(:ports, DB.Port)
  end

  def changeset(device, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(device, Enum.into(params, %{}), all_str)
    device
    |> cast(params_, [:name, :ip, :port, :type_id, :alive, :ref])
  end

  def preload, do: [device: :type]

  def get(id) do
    Repo.get(Device, id)
    |> Repo.preload(:type)
  end

  def get_by_name(name) do
    Repo.get_by(Device, name: name)
    |> Repo.preload(:type)
  end

  def get_by_type(id) do
    from(d in Device, where: d.type_id == ^id)
    |> Repo.all()
  end

  def all() do
    Repo.all(Device)
    |> Repo.preload(:type)
  end
end
