defmodule DB.Proc.ActionListProc do
  @doc false

  use GenServer

  alias DB.Data.Action
  alias DB.Data.Port

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @spec add(map() | Action.t()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def add(params) do
    GenServer.call(__MODULE__, {:insert, params})
  end

  @spec update(integer(), map() | Action.t()) :: {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def update(action_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:update, action_id, params})
  end

  @spec fast_update(integer(), map() | Action.t()) ::
          {:ok, Action.t()} | {:error, Ecto.Changeset.t()}
  def fast_update(action_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:not_persistent_update, action_id, params})
  end

  @spec get(integer()) :: {:ok, Action.t()} | {:error, term()}
  def get(action_id) do
    GenServer.call(__MODULE__, {:get, action_id})
  end

  @spec list_active() :: {:ok, [Action.t()]} | {:error, term()}
  def list_active() do
    GenServer.call(__MODULE__, :all_active)
  end

  @spec get_by_activator([Port.t()]) :: {:ok, [Action.t()]} | {:error, term()}
  def get_by_activator(ports) do
    GenServer.call(__MODULE__, {:get_by_activator, ports})
  end

  @spec list_all() :: {:ok, [Action.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @type state_t() :: %{actions: [Action]}

  @impl true
  def init(_init_arg) do
    actions =
      Action.list_all()
      |> Enum.map(&{&1.id, &1})
      |> Enum.into(%{})

    {:ok, %{actions: actions}}
  end

  @impl true
  def handle_call(:all, _from, %{actions: actions} = state) do
    return = Enum.map(actions, fn {_k, v} -> v end)
    {:reply, {:ok, return}, state}
  end

  @impl true
  def handle_call(:all_active, _from, %{actions: actions} = state) do
    return = Enum.filter(actions, fn {_k, v} -> v.state end) |> Enum.map(fn {_k, v} -> v end)
    {:reply, {:ok, return}, state}
  end

  @impl true
  def handle_call({:get, id}, _from, %{actions: actions} = state) do
    return =
      case Map.get(actions, id) do
        nil -> {:error, :not_found}
        action -> {:ok, action}
      end

    {:reply, return, state}
  end

  @impl true
  def handle_call({:get_by_activator, ports}, _from, %{actions: actions_map} = state) do
    ports_ids = Enum.map(ports, & &1.id)

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

  @impl true
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

  @impl true
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

  @impl true
  def handle_call({:insert, params}, _from, state) do
    with {:ok, action} <- Action.insert(params) do
      {:reply, {:ok, action}, put_action(state, action)}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  defp get_action(%{actions: actions}, action_id) do
    case Map.get(actions, action_id) do
      nil -> {:error, :not_found}
      action -> {:ok, action}
    end
  end

  defp put_action(%{actions: actions} = state, action) do
    %{state | actions: Map.put(actions, action.id, action)}
  end
end
