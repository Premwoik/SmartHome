defmodule DB.Proc.PortListProc do
  @doc false

  use GenServer

  alias DB.Proc.Beh, as: ProcBeh
  alias DB.Data.Port

  @behaviour ProcBeh

  @type resp() :: ProcBeh.resp(Port.t())
  @type resp!() :: ProcBeh.resp!(Port.t())
  @type id() :: ProcBeh.item_id()

  @type state_t() :: %{ports: [Port]}

  ## Process Behaviour API

  @impl ProcBeh
  @doc ""
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl ProcBeh
  @doc ""
  @spec update_state(integer(), map()) :: resp()
  def update_state(port_id, params) do
    GenServer.call(__MODULE__, {:update_state, port_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec update_state!(integer(), map()) :: resp!()
  def update_state!(port_id, params) do
    ProcBeh.get_just(update_state(port_id, params))
  end

  @impl ProcBeh
  @doc ""
  @spec update(integer(), map() | Port.t()) :: resp()
  def update(port_id, params) do
    GenServer.call(__MODULE__, {:update, port_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec update!(integer(), map() | Port.t()) :: resp!()
  def update!(port_id, params) do
    ProcBeh.get_just(update(port_id, params))
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update(integer(), map() | Port.t()) :: resp()
  def fast_update(port_id, params) do
    GenServer.call(__MODULE__, {:not_persistent_update, port_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update!(integer(), map() | Port.t()) :: resp!()
  def fast_update!(port_id, params) do
    ProcBeh.get_just(fast_update(port_id, params))
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update_state(integer(), map() | Port.t()) :: resp()
  def fast_update_state(port_id, params) do
    GenServer.call(__MODULE__, {:not_persistent_state_update, port_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update_state!(integer(), map() | Port.t()) :: resp!()
  def fast_update_state!(port_id, params) do
    ProcBeh.get_just(fast_update_state(port_id, params))
  end

  @spec fast_update_inputs([integer()], map()) :: [Port.t()]
  def fast_update_inputs(port_ids, params) do
    GenServer.call(__MODULE__, {:not_persistent_inputs_update, port_ids, params})
  end

  @impl ProcBeh
  @doc ""
  @spec get(id()) :: resp()
  def get(port_id) do
    GenServer.call(__MODULE__, {:get, port_id})
  end

  @impl ProcBeh
  @doc ""
  @spec get!(id()) :: resp!()
  def get!(port_id) do
    ProcBeh.get_just(get(port_id))
  end

  @impl ProcBeh
  @doc ""
  @spec list_all() :: {:ok, [Port.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @impl ProcBeh
  @doc ""
  @spec list_all!() :: [Port.t()]
  def list_all!() do
    ProcBeh.get_just(list_all(), [])
  end

  @impl ProcBeh
  @doc ""
  @spec force_read_all() :: :ok
  def force_read_all() do
    GenServer.cast(__MODULE__, {:force_read, :all})
  end

  ## Custom process API

  def force_read_id(id) do
    GenServer.cast(__MODULE__, {:force_read, {:id, id}})
  end

  @spec get_ids([integer()]) :: [Port.t()]
  def get_ids(port_ids) do
    ProcBeh.get_just(GenServer.call(__MODULE__, {:get_ids, port_ids}))
  end

  @spec add(map() | Port.t()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def add(params) do
    GenServer.call(__MODULE__, {:insert, params})
  end

  @spec add!(map() | Port.t()) :: nil | Port.t()
  def add!(params) do
    ProcBeh.get_just(add(params))
  end

  @spec identify(integer(), integer() | [integer()]) :: resp() | [resp()]
  def identify(device_id, numbers) when is_list(numbers) do
    GenServer.call(__MODULE__, {:identify, device_id, numbers})
  end

  def identify(device_id, number) do
    GenServer.call(__MODULE__, {:identify, device_id, [number]})
  end

  @spec identify!(integer(), integer() | [integer()]) :: resp!() | [resp!()]
  def identify!(device_id, numbers) do
    ProcBeh.get_just(identify(device_id, numbers), [])
  end

  ## GenServer API

  @impl GenServer
  def init(_init_arg) do
    ports = read_all_from_db()
    {:ok, %{ports: ports}}
  end

  @impl GenServer
  def handle_call(:all, _from, %{ports: ports} = state) do
    {:reply, {:ok, Map.values(ports)}, state}
  end

  @impl GenServer
  def handle_call({:get, id}, _from, state) do
    {:reply, get_port(state, id), state}
  end

  @impl GenServer
  def handle_call({:identify, device_id, numbers}, _from, %{ports: ports_map} = state) do
    return =
      case Port.identify(Map.values(ports_map), device_id, numbers) do
        [] -> {:error, :not_found}
        ports -> {:ok, ports}
      end

    {:reply, return, state}
  end

  @impl GenServer
  def handle_call({:get_ids, ids}, _from, %{ports: ports} = state) do
    {:reply, {:ok, Map.values(Map.take(ports, ids))}, state}
  end

  @impl GenServer
  def handle_call({:update_state, port_id, state_params}, _from, state) do
    with {:ok, port} <- get_port(state, port_id),
         params <- %{state: Map.merge(port.state, state_params)},
         {:ok, updated_port} <- Port.update(port, params) do
      new_state = put_port(state, updated_port)
      {:reply, {:ok, updated_port}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:update, port_id, params}, _from, state) do
    with {:ok, port} <- get_port(state, port_id),
         {:ok, updated_port} <- Port.update(port, params) do
      new_state = put_port(state, updated_port)
      {:reply, {:ok, updated_port}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:not_persistent_update, port_id, params}, _from, state) do
    with {:ok, port} <- get_port(state, port_id),
         {:ok, updated_port} <- Port.virtual_update(port, params) do
      new_state = put_port(state, updated_port)
      {:reply, {:ok, updated_port}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:not_persistent_state_update, port_id, state_params}, _from, state) do
    with {:ok, port} <- get_port(state, port_id),
         params <- %{state: Map.merge(port.state, state_params)},
         {:ok, updated_port} <- Port.virtual_update(port, params) do
      new_state = put_port(state, updated_port)
      {:reply, {:ok, updated_port}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call(
        {:not_persistent_inputs_update, port_ids, state_params},
        _from,
        %{ports: ports} = state
      ) do
    new_ports =
      Enum.map(ports, fn {id, port} ->
        if id in port_ids do
          params = %{state: Map.merge(port.state, state_params)}
          {:ok, updated_port} = Port.virtual_update(port, params)
          {id, updated_port}
        else
          {id, port}
        end
      end)

    {:reply, {:ok, new_ports}, %{state | ports: new_ports}}
  end

  @impl GenServer
  def handle_call({:insert, params}, _from, state) do
    with {:ok, port} <- Port.insert(params) do
      {:reply, {:ok, port}, put_port(state, port)}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_cast({:force_read, :all}, state) do
    {:noreply, %{state | ports: read_all_from_db()}}
  end

  @impl true
  def handle_cast({:force_read, {:id, id}}, state) do
    {:ok, port} = Map.fetch(read_all_from_db(), id)
    {:noreply, put_port(state, port)}
  end

  ## Internal helpers

  defp get_port(%{ports: ports}, port_id) do
    ProcBeh.get_maybe(ports[port_id])
  end

  defp put_port(%{ports: ports} = state, port) do
    %{state | ports: Map.put(ports, port.id, port)}
  end

  defp read_all_from_db() do
    Port.list_all()
    |> Enum.map(&{&1.id, &1})
    |> Enum.into(%{})
  end
end
