defmodule DB.Proc.DeviceListProc do
  @doc false

  use GenServer

  alias DB.Proc.Beh, as: ProcBeh
  alias DB.Data.Device

  @behaviour ProcBeh

  @type resp() :: ProcBeh.resp(Device.t())
  @type resp!() :: ProcBeh.resp!(Device.t())
  @type id() :: ProcBeh.item_id()

  @type state_t() :: %{devices: [Device]}

  ## API

  @impl ProcBeh
  @doc ""
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl ProcBeh
  @doc ""
  @spec get(id()) :: resp()
  def get(device_id) do
    ProcBeh.get_maybe(get!(device_id))
  end

  @spec get_ids([id()]) :: [Device.t()]
  def get_ids(device_ids) do
    GenServer.call(__MODULE__, {:get_ids, device_ids})
  end

  @impl ProcBeh
  @doc ""
  @spec get!(integer()) :: resp!()
  def get!(device_id) do
    GenServer.call(__MODULE__, {:get, device_id})
  end

  @impl ProcBeh
  @doc ""
  @spec list_all() :: {:ok, [Device.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @impl ProcBeh
  @doc ""
  @spec list_all!() :: [Device.t()]
  def list_all!() do
    list_all()
    |> ProcBeh.get_just([])
  end

  @impl ProcBeh
  @doc ""
  @spec force_read_all() :: :ok
  def force_read_all() do
    GenServer.cast(__MODULE__, {:force_read, :all})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update(id(), map() | Device.t()) :: resp()
  def fast_update(device_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:not_persistent_update, device_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update!(id(), map() | Device.t()) :: resp!()
  def fast_update!(device_id, params) do
    fast_update(device_id, params)
    |> ProcBeh.get_just()
  end

  ## Genserver handlers 

  @impl GenServer
  def init(_init_arg) do
    devices = read_all_from_db()
    {:ok, %{devices: devices}}
  end

  @impl GenServer
  def handle_call(:all, _from, %{devices: devices} = state) do
    return = Map.values(devices)
    {:reply, {:ok, return}, state}
  end

  @impl GenServer
  def handle_call({:get, id}, _from, %{devices: devices} = state) do
    {:reply, devices[id], state}
  end

  @impl GenServer
  def handle_call({:get_ids, ids}, _from, %{devices: devices} = state) do
    {:reply, Map.values(Map.take(devices, ids)), state}
  end

  @impl GenServer
  def handle_call({:update, device_id, params}, _from, state) do
    with {:ok, device} <- get_device(state, device_id),
         {:ok, updated_device} <- Device.update(device, params) do
      new_state = put_device(state, updated_device)
      {:reply, {:ok, updated_device}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:not_persistent_update, device_id, params}, _from, state) do
    with {:ok, device} <- get_device(state, device_id),
         {:ok, updated_device} <- Device.virtual_update(device, params) do
      new_state = put_device(state, updated_device)
      {:reply, {:ok, updated_device}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_cast({:force_read, :all}, state) do
    {:noreply, %{state | devices: read_all_from_db()}}
  end

  ## Internal helpers

  defp get_device(%{devices: devices}, device_id) do
    ProcBeh.get_maybe(devices[device_id])
  end

  defp put_device(%{devices: devices} = state, device) do
    %{state | devices: Map.put(devices, device.id, device)}
  end

  @spec read_all_from_db() :: map()
  defp read_all_from_db() do
    Device.list_all!()
    |> Enum.map(&{&1.id, &1})
    |> Enum.into(%{})
  end
end
