defmodule DB.Data.Action do
  @moduledoc """
  The action data.
  """
  use Ecto.Schema
  import Ecto.Changeset

  alias DB.Data.Action
  alias DB.Data.Device
  alias DB.Data.Port
  alias DB.MainRepo
  alias DB.Proc.PortListProc

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %Action{
          id: integer(),
          name: String.t(),
          state: boolean(),
          start_time: nil | Time.t(),
          end_time: nil | Time.t(),
          pause: integer(),
          attributes: map(),
          module: String.t(),
          activator_id: integer(),
          activator: Port.t()
        }
  schema "actions" do
    field(:name, :string)
    field(:state, :boolean)
    field(:start_time, :time)
    field(:end_time, :time)
    field(:pause, :integer)
    field(:module, :string)
    field(:attributes, :map)

    belongs_to(:activator, Port)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :attributes])
    |> foreign_key_constraint(:activator_id)
  end

  @spec list_all() :: [Action.t()]
  def list_all() do
    MainRepo.all(Action) |> MainRepo.preload([:activator])
  end

  @spec update(Action.t(), map()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def update(action, params) do
    changeset(action, params)
    |> MainRepo.update()
  end

  @spec virtual_update(Action.t(), map()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def virtual_update(action, params) do
    with %Ecto.Changeset{valid?: true, data: data, changes: changes} <- changeset(action, params) do
      new_data = Map.merge(Map.from_struct(data), changes)
      {:ok, struct(%Action{}, new_data)}
    else
      error_changeset ->
        {:error, error_changeset}
    end
  end

  @spec insert(map()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def insert(params) do
    changeset(%Action{}, params)
    |> MainRepo.insert()
  end

  @spec arguments(Action.t(), nil | :up | :down) :: [Port.t()]
  def arguments(action, up_down) do
    ids =
      case up_down do
        :up -> action.attributes["up"]
        :down -> action.attributes["down"]
      end

    {:ok, ports} = PortListProc.get_ids(ids)
    ports
  end

  @spec get_ports(Action.t(), String.t()) :: [Port.t()]
  def get_ports(action, str_key) do
    ids = action.attributes[str_key]

    {:ok, ports} = PortListProc.get_ids(ids)
    ports
  end

  @spec get_device(Action.t()) :: [Device.t()]
  def get_device(action) do
    device_id = action.attributes["device"]
    {:ok, device} = Device.find(device_id)
    device
  end
end
