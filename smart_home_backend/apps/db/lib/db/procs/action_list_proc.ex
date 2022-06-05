defmodule DB.Proc.ActionListProc do
  @doc false

  use GenServer

  alias DB.Data.Action
  alias DB.Data.Port
  alias DB.Proc.Beh, as: ProcBeh

  @behaviour ProcBeh

  @type resp() :: ProcBeh.resp(Action.t())
  @type resp!() :: ProcBeh.resp!(Action.t())
  @type id() :: ProcBeh.item_id()

  @type state_t() :: %{actions: [Action]}

  ## API

  @impl ProcBeh
  @doc ""
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl ProcBeh
  @doc ""
  @spec update(id(), map() | Action.t()) :: resp()
  def update(action_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:update, action_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec update!(id(), map() | Action.t()) :: resp!()
  def update!(action_id, params) do
    ProcBeh.get_just(update(action_id, params))
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update(id(), map() | Action.t()) :: resp()
  def fast_update(action_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:not_persistent_update, action_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update!(id(), map() | Action.t()) :: resp!()
  def fast_update!(action_id, params) do
    ProcBeh.get_just(fast_update(action_id, params))
  end

  @impl ProcBeh
  @doc ""
  @spec get!(id()) :: resp!()
  def get!(action_id) do
    ProcBeh.get_just(get(action_id))
  end

  @impl ProcBeh
  @doc ""
  @spec get(id()) :: resp()
  def get(action_id) do
    GenServer.call(__MODULE__, {:get, action_id})
  end

  @impl ProcBeh
  @doc ""
  @spec list_all() :: {:ok, [Action.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @impl ProcBeh
  @doc ""
  @spec list_all!() :: [Action.t()]
  def list_all!() do
    ProcBeh.get_just(list_all!(), [])
  end

  @impl ProcBeh
  @doc ""
  @spec force_read_all() :: :ok
  def force_read_all() do
    GenServer.cast(__MODULE__, {:force_read, :all})
  end

  ## Custom API

  def force_read_id(id) do
    GenServer.cast(__MODULE__, {:force_read, {:id, id}})
  end

  @spec get_ids([integer()]) :: [Action.t()]
  def get_ids(action_ids) do
    GenServer.call(__MODULE__, {:get_ids, action_ids})
  end

  @spec add(map() | Action.t()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def add(params) do
    GenServer.call(__MODULE__, {:insert, params})
  end

  @spec list_active() :: {:ok, [Action.t()]} | {:error, term()}
  def list_active() do
    GenServer.call(__MODULE__, :all_active)
  end

  @spec get_by_activator([Port.t()]) :: {:ok, [Action.t()]} | {:error, term()}
  def get_by_activator(ports) do
    ports_ids = Enum.map(ports, & &1.id)
    GenServer.call(__MODULE__, {:get_by_activator, ports_ids})
  end

  ## GenServer API

  @impl GenServer
  def init(_init_arg) do
    {:ok, %{actions: read_all_from_db()}}
  end

  @impl GenServer
  def handle_call(:all, _from, %{actions: actions} = state) do
    {:reply, {:ok, Map.values(actions)}, state}
  end

  @impl GenServer
  def handle_call(:all_active, _from, %{actions: actions} = state) do
    return = Map.values(actions) |> Enum.filter(fn a -> a.state end)
    {:reply, {:ok, return}, state}
  end

  @impl GenServer
  def handle_call({:get, id}, _from, state) do
    {:reply, get_action(state, id), state}
  end

  @impl GenServer
  def handle_call({:get_ids, ids}, _from, %{actions: actions} = state) do
    {:reply, Map.values(Map.take(actions, ids)), state}
  end

  @impl GenServer
  def handle_call({:get_by_activator, ports_ids}, _from, %{actions: actions_map} = state) do
    return =
      actions_map
      |> Map.values()
      |> Enum.filter(fn %{activator_id: id} -> id in ports_ids end)
      |> case do
        [] -> {:error, :not_found}
        actions -> {:ok, actions}
      end

    {:reply, return, state}
  end

  @impl GenServer
  def handle_call({:update, action_id, params}, _from, state) do
    with {:ok, action} <- get_action(state, action_id),
         {:ok, updated_action} <- Action.update(action, params) do
      new_state = put_action(state, updated_action)
      {:reply, {:ok, updated_action}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:not_persistent_update, action_id, params}, _from, state) do
    with {:ok, action} <- get_action(state, action_id),
         {:ok, updated_action} <- Action.virtual_update(action, params) do
      new_state = put_action(state, updated_action)
      {:reply, {:ok, updated_action}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:insert, params}, _from, state) do
    with {:ok, action} <- Action.insert(params) do
      {:reply, {:ok, action}, put_action(state, action)}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_cast({:force_read, :all}, state) do
    {:noreply, %{state | actions: read_all_from_db()}}
  end

  @impl GenServer
  def handle_cast({:force_read, {:id, id}}, state) do
    {:ok, action} = Map.fetch(read_all_from_db(), id)
    {:noreply, put_action(state, action)}
  end

  ## Internal helpers

  defp get_action(%{actions: actions}, action_id) do
    ProcBeh.get_maybe(actions[action_id])
  end

  defp put_action(%{actions: actions} = state, action) do
    %{state | actions: Map.put(actions, action.id, action)}
  end

  @spec read_all_from_db() :: map()
  defp read_all_from_db() do
    Action.list_all()
    |> Enum.map(&{&1.id, &1})
    |> Enum.into(%{})
  end
end
