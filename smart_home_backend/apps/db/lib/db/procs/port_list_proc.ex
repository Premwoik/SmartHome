defmodule DB.Proc.PortListProc do
  @doc false

  use GenServer

  alias DB.Data.Port

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @spec add(map() | Port.t()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def add(params) do
    GenServer.call(__MODULE__, {:insert, params})
  end

  @spec add!(map() | Port.t()) :: nil | Port.t()
  def add!(params) do
    add(params) |> get_just()
  end

  @spec update(integer(), map() | Port.t()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def update(port_id, params) do
    GenServer.call(__MODULE__, {:update, port_id, params})
  end

  @spec update!(integer(), map() | Port.t()) :: nil | Port.t()
  def update!(port_id, params) do
    update(port_id, params) |> get_just()
  end

  @spec fast_update(integer(), map() | Port.t()) :: {:ok, Port.t()} | {:error, Ecto.Changeset.t()}
  def fast_update(port_id, params) do
    GenServer.call(__MODULE__, {:not_persistent_update, port_id, params})
  end

  @spec fast_update!(integer(), map() | Port.t()) :: nil | Port.t()
  def fast_update!(port_id, params) do
    fast_update(port_id, params) |> get_just()
  end

  @spec get(integer()) :: {:ok, Port.t()} | {:error, term()}
  def get(port_id) do
    GenServer.call(__MODULE__, {:get, port_id})
  end

  @spec get!(integer()) :: nil | Port.t()
  def get!(port_id) do
    get(port_id) |> get_just()
  end

  @spec identify(integer(), integer() | [integer()]) :: {:ok, [Port.t()]} | {:error, term()}
  def identify(device_id, numbers) when is_list(numbers) do
    GenServer.call(__MODULE__, {:identify, device_id, numbers})
  end

  def identify(device_id, number) do
    GenServer.call(__MODULE__, {:identify, device_id, [number]})
  end

  @spec identify!(integer(), integer() | [integer()]) :: nil | Port.t()
  def identify!(device_id, number) do
    identify(device_id, number) |> get_just()
  end

  @spec get_ids([integer()]) :: {:ok, [Port.t()]} | {:error, term()}
  def get_ids(port_ids) do
    GenServer.call(__MODULE__, {:get_ids, port_ids})
  end

  @spec get_ids!([integer()]) :: [Port.t()]
  def get_ids!(port_ids) do
    get_ids(port_ids) |> get_just()
  end

  @spec list_all() :: {:ok, [Port.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @spec list_all!() :: [Port.t()]
  def list_all!() do
    list_all() |> get_just([])
  end

  @type state_t() :: %{ports: [Port]}

  @impl true
  def init(_init_arg) do
    ports =
      Port.list_all()
      |> Enum.map(&{&1.id, &1})
      |> Enum.into(%{})

    {:ok, %{ports: ports}}
  end

  @impl true
  def handle_call(:all, _from, %{ports: ports} = state) do
    return = Enum.map(ports, fn {_k, v} -> v end)
    {:reply, {:ok, return}, state}
  end

  @impl true
  def handle_call({:get, id}, _from, %{ports: ports} = state) do
    return =
      case Map.get(ports, id) do
        nil -> {:error, :not_found}
        port -> {:ok, port}
      end

    {:reply, return, state}
  end

  @impl true
  def handle_call({:identify, device_id, numbers}, _from, %{ports: ports_map} = state) do
    return =
      case Port.identify(Map.values(ports_map), device_id, numbers) do
        [] -> {:error, :not_found}
        ports -> {:ok, ports}
      end

    {:reply, return, state}
  end

  @impl true
  def handle_call({:get_ids, ids}, _from, %{ports: ports} = state) do
    return = {:ok, Enum.filter(ports, fn p -> p.id in ids end)}

    {:reply, return, state}
  end

  @impl true
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

  @impl true
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

  @impl true
  def handle_call({:insert, params}, _from, state) do
    with {:ok, port} <- Port.insert(params) do
      {:reply, {:ok, port}, put_port(state, port)}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  defp get_port(%{ports: ports}, port_id) do
    case Map.get(ports, port_id) do
      nil -> {:error, :not_found}
      port -> {:ok, port}
    end
  end

  defp put_port(%{ports: ports} = state, port) do
    %{state | ports: Map.put(ports, port.id, port)}
  end

  defp get_just(res, def_ \\ nil) do
    case res do
      {:ok, res} -> res
      _ -> def_
    end
  end
end
