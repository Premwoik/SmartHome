defmodule DB.Data.Port do
  @typedoc """
  The port data.
  """
  use Ecto.Schema
  import Ecto.Changeset
  # import Ecto.Query

  alias DB.Data.Device
  alias DB.Data.Port
  alias DB.MainRepo

  @type mode_t :: :input | :output | :input_output
  @type type_t :: :light | :dimmer | :sunblind | :custom | :signal | :motion_sensor

  @typedoc """

  """
  @type state_t ::
          %{:value => integer()}
          # dimmer 
          | %{:value => integer(), :lights => [light_t()]}

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %Port{
          id: integer(),
          name: String.t(),
          number: integer(),
          mode: mode_t(),
          type: type_t(),
          state: state_t(),
          device_id: integer(),
          device: Device.t()
        }

  @type light_t :: t()
  @type dimmer_t :: t()
  @type sunblind_t :: t()

  schema "ports" do
    field(:name, :string)
    field(:number, :integer)
    field(:mode, Ecto.Enum, values: [:input, :output, :input_output], default: :output)

    field(:type, Ecto.Enum,
      values: [:light, :dimmer, :sunblind, :custom, :signal, :motion_sensor],
      default: :custom
    )

    field(:state, :map)

    belongs_to(:device, Device)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :number, :state, :device_id])
    |> foreign_key_constraint(:device_id)
  end

  @spec list_all() :: [%Port{}]
  def list_all() do
    MainRepo.all(Port) |> MainRepo.preload([:device])
  end

  # @spec find_port(integer()) :: nil | Port.t()
  # def find_port(port_id) do
  # MainRepo.get(Port, port_id) |> MainRepo.preload([:device])
  # end

  @spec update(%Port{}, map()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def update(port, params) do
    changeset(port, params)
    |> MainRepo.update()
  end

  @spec insert(map()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def insert(params) do
    changeset(%Port{}, params)
    |> MainRepo.insert()
  end

  def virtual_update(port, params) do
    with %Ecto.Changeset{valid?: true, data: data, changes: changes} <- changeset(port, params) do
      new_data = Map.merge(Map.from_struct(data), changes)
      {:ok, struct(%Port{}, new_data)}
    else
      error_changeset ->
        {:error, error_changeset}
    end
  end

  @spec any_on?([Port.t()]) :: boolean()
  def any_on?(ports) do
    Enum.find(ports, fn %Port{state: %{"value" => value}} -> value end)
  end

  @spec identify([Port.t()], integer(), [integer()]) :: [Port.t()]
  def identify(ports, device_id, numbers) do
    Enum.filter(ports, fn p -> p.device_id == device_id and p.number in numbers end)
  end
end
