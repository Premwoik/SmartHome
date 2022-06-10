defmodule DB.Proc.RfButtonListProc do
  @doc false

  use GenServer

  alias DB.Proc.Beh, as: ProcBeh
  alias DB.Data.RfButton

  @behaviour ProcBeh

  @type resp() :: ProcBeh.resp(RfButton.t())
  @type resp!() :: ProcBeh.resp!(RfButton.t())
  @type id() :: ProcBeh.item_id()

  @type state_t() :: %{buttons: [RfButton.t()]}

  ## API

  @impl ProcBeh
  @doc ""
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl ProcBeh
  @doc ""
  @spec get(id()) :: resp()
  def get(btn_id) do
    ProcBeh.get_maybe(get!(btn_id))
  end

  @spec get_ids([id()]) :: [RfButton.t()]
  def get_ids(btn_id) do
    GenServer.call(__MODULE__, {:get_ids, btn_id})
  end

  @impl ProcBeh
  @doc ""
  @spec get!(id()) :: resp!()
  def get!(btn_id) do
    GenServer.call(__MODULE__, {:get, btn_id})
  end

  @impl ProcBeh
  @doc ""
  @spec list_all() :: {:ok, [RfButton.t()]}
  def list_all() do
    GenServer.call(__MODULE__, :all)
  end

  @impl ProcBeh
  @doc ""
  @spec list_all!() :: [RfButton.t()]
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
  @spec fast_update(id(), map() | RfButton.t()) :: resp()
  def fast_update(btn_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:not_persistent_update, btn_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec fast_update!(id(), map() | RfButton.t()) :: resp!()
  def fast_update!(btn_id, params) do
    fast_update(btn_id, params)
    |> ProcBeh.get_just()
  end

  @spec identify(String.t() | [String.t()]) :: resp() | [resp()]
  def identify(numbers) when is_list(numbers) do
    GenServer.call(__MODULE__, {:identify, numbers})
  end

  def identify(number) do
    GenServer.call(__MODULE__, {:identify, [number]})
  end

  @spec identify!(String.t() | [String.t()]) :: resp!() | [resp!()]
  def identify!(numbers) when is_list(numbers) do
    ProcBeh.get_just(identify(numbers), [])
  end

  def identify!(number) do
    ProcBeh.get_just(identify(number), nil)
  end

  ## Genserver handlers 

  @impl GenServer
  def init(_init_arg) do
    buttons = read_all_from_db()
    {:ok, %{buttons: buttons}}
  end

  @impl GenServer
  def handle_call(:all, _from, %{buttons: buttons} = state) do
    return = Map.values(buttons)
    {:reply, {:ok, return}, state}
  end

  @impl GenServer
  def handle_call({:get, id}, _from, %{buttons: buttons} = state) do
    {:reply, buttons[id], state}
  end

  @impl GenServer
  def handle_call({:get_ids, ids}, _from, %{devices: buttons} = state) do
    {:reply, Map.values(Map.take(buttons, ids)), state}
  end

  @impl GenServer
  def handle_call({:update, btn_id, params}, _from, state) do
    with {:ok, btn} <- get_btn(state, btn_id),
         {:ok, updated_btn} <- RfButton.update(btn, params) do
      new_state = put_btn(state, updated_btn)
      {:reply, {:ok, updated_btn}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:not_persistent_update, btn_id, params}, _from, state) do
    with {:ok, btn} <- get_btn(state, btn_id),
         {:ok, updated_btn} <- RfButton.virtual_update(btn, params) do
      new_state = put_btn(state, updated_btn)
      {:reply, {:ok, updated_btn}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_call({:identify, numbers}, _from, %{buttons: btns_map} = state) do
    return =
      case RfButton.identify(Map.values(btns_map), numbers) do
        [] -> {:error, :not_found}
        ports -> {:ok, ports}
      end

    {:reply, return, state}
  end

  @impl GenServer
  def handle_cast({:force_read, :all}, state) do
    {:noreply, %{state | buttons: read_all_from_db()}}
  end

  ## Internal helpers

  defp get_btn(%{buttons: buttons}, btn_id) do
    ProcBeh.get_maybe(buttons[btn_id])
  end

  defp put_btn(%{buttons: buttons} = state, button) do
    %{state | button: Map.put(buttons, button.id, button)}
  end

  @spec read_all_from_db() :: map()
  defp read_all_from_db() do
    RfButton.list_all!()
    |> Enum.map(&{&1.id, &1})
    |> Enum.into(%{})
  end
end
