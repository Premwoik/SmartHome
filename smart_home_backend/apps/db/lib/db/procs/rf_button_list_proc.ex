defmodule DB.Proc.RfButtonListProc do
  @doc false

  use GenServer

  alias DB.Proc.Beh, as: ProcBeh
  alias DB.Data.RfButton
  alias DB.Data.RemoteController

  @behaviour ProcBeh

  @type resp() :: ProcBeh.resp(RfButton.t())
  @type resp!() :: ProcBeh.resp!(RfButton.t())
  @type resp_list() :: ProcBeh.resp([RfButton.t()])
  @type resp_list!() :: ProcBeh.resp!([RfButton.t()])
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

  @doc ""
  @spec list_all_controllers() :: {:ok, [RemoteController.t()]}
  def list_all_controllers() do
    GenServer.call(__MODULE__, :all_controllers)
  end

  @doc ""
  @spec list_all_controllers!() :: [RemoteController.t()]
  def list_all_controllers!() do
    list_all_controllers()
    |> ProcBeh.get_just([])
  end

  @impl ProcBeh
  @doc ""
  @spec force_read_all() :: :ok
  def force_read_all() do
    GenServer.cast(__MODULE__, {:force_read, :all})
  end

  @spec update_action(integer(), String.t(), map()) :: resp()
  def update_action(port_id, page, params) do
    GenServer.call(__MODULE__, {:update_action, port_id, page, params})
  end

  @spec update_action!(integer(), String.t(), map()) :: resp!()
  def update_action!(port_id, btn, params) do
    ProcBeh.get_just(update_action(port_id, btn, params))
  end

  @impl ProcBeh
  @doc ""
  @spec update(id(), map() | RfButton.t()) :: resp()
  def update(btn_id, params) do
    params = Map.delete(params, :__struct__)
    GenServer.call(__MODULE__, {:update, btn_id, params})
  end

  @impl ProcBeh
  @doc ""
  @spec update!(id(), map() | RfButton.t()) :: resp!()
  def update!(btn_id, params) do
    update(btn_id, params)
    |> ProcBeh.get_just()
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

  @spec identify([String.t()]) :: resp_list()
  def identify(numbers) when is_list(numbers) do
    GenServer.call(__MODULE__, {:identify, numbers})
  end

  @spec identify!([String.t()]) :: resp_list!()
  def identify!(numbers) when is_list(numbers) do
    ProcBeh.get_just(identify(numbers), [])
  end

  ## Genserver handlers 

  @impl GenServer
  def init(_init_arg) do
    buttons = read_all_from_db(RfButton)
    controllers = read_all_from_db(RemoteController)
    {:ok, %{buttons: buttons, controllers: controllers}}
  end

  @impl GenServer
  def handle_call(:all, _from, %{buttons: buttons} = state) do
    return = Map.values(buttons)
    {:reply, {:ok, return}, state}
  end

  @impl GenServer
  def handle_call(:all_controllers, _from, %{controllers: controllers, buttons: buttons} = state) do
    return =
      Map.values(controllers)
      |> Enum.map(&controller_with_buttons(&1, state))

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
  def handle_call({:update_action, btn_id, page, page_params}, _from, state) do
    with {:ok, %{on_click_action: action} = btn} <- get_btn(state, btn_id),
         params <- %{on_click_action: put_in(action["pages"][page], page_params)},
         {:ok, updated_btn} <- RfButton.update(btn, params) do
      new_state = put_btn(state, updated_btn)
      {:reply, {:ok, updated_btn}, new_state}
    else
      {:error, _} = error ->
        {:reply, error, state}
    end
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
    buttons = read_all_from_db(RfButton)
    controllers = read_all_from_db(RemoteController)
    {:noreply, %{state | buttons: buttons, controllers: controllers}}
  end

  ## Internal helpers

  defp get_btn(%{buttons: buttons}, btn_id) do
    ProcBeh.get_maybe(buttons[btn_id])
  end

  defp put_btn(%{buttons: buttons} = state, button) do
    %{state | buttons: Map.put(buttons, button.id, button)}
  end

  defp controller_with_buttons(%RemoteController{id: id} = c, %{buttons: buttons}) do
    filtered = for {_, rf = %RfButton{controller_id: ^id}} <- buttons, do: rf
    %RemoteController{c | buttons: filtered}
  end

  @spec read_all_from_db(module()) :: map()
  defp read_all_from_db(mod) do
    mod.list_all!()
    |> Enum.map(&{&1.id, &1})
    |> Enum.into(%{})
  end
end
