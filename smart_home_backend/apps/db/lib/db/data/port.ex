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

  @common_valid_keys ["value"]
  @common_required %{"value" => false}

  @spec normalize_state(type :: atom(), params :: map()) :: map()
  def normalize_state(:light, params) do
    valid_keys = ["dimmer_id"]
    requested = %{"dimmer_id" => nil}
    normalize_state2(valid_keys, requested, params)
  end

  def normalize_state(:dimmer, params) do
    valid_keys = ["light_ids", "brightness", "white", "color", "subtype"]
    requested = %{"light_ids" => []}
    normalize_state2(valid_keys, requested, params)
  end

  def normalize_state(:sunblind, params) do
    valid_keys = ["move_duration", "position"]
    requested = %{"move_duration" => 30_000, "position" => "open"}
    normalize_state2(valid_keys, requested, params)
  end

  def normalize_state(_, params) do
    normalize_state2([], %{}, params)
  end

  def normalize_state2(valid_keys, requested, params) do
    valid_keys = valid_keys ++ @common_valid_keys
    requested = Map.merge(@common_required, requested)

    params =
      params
      |> Enum.filter(fn {key, _} -> key in valid_keys end)
      |> Map.new()

    requested
    |> Map.keys()
    |> Enum.reduce(params, fn key, acc ->
      if Map.has_key?(acc, key) do
        acc
      else
        Map.put(acc, key, Map.get(acc, key))
      end
    end)
  end

  @spec put_state(Port.t(), String.t(), any()) :: Port.t()
  def put_state(port, key, value) do
    state = Map.put(port.state, key, value)
    Map.put(port, :state, state)
  end

  def state_value_changed(port, key, value) do
    Map.get(port.state, key) != value
  end

  @spec list_all() :: [Port.t()]
  def list_all() do
    MainRepo.all(Port) |> MainRepo.preload([:device])
  end

  # @spec find_port(integer()) :: nil | Port.t()
  # def find_port(port_id) do
  # MainRepo.get(Port, port_id) |> MainRepo.preload([:device])
  # end

  @spec update(%Port{}, map()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def update(port, params) do
    state0 = Map.get(params, :state, %{})
    state = normalize_state(port.type, state0)

    params2 =
      Map.put(params, :state, state)
      |> Map.delete(:__struct__)

    changeset(port, params2)
    |> MainRepo.update()
    |> merge_untracked_state(state0)
  end

  defp merge_untracked_state({:ok, port}, state) do
    new_state = Map.merge(state, port.state)
    {:ok, %{port | state: new_state}}
  end

  defp merge_untracked_state(error, _state) do
    error
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
