defmodule UiWeb.DashboardChannel.Helper do
  @moduledoc false

  use GenServer


  @impl true
  def init(stack) do
    {:ok, stack}
  end

  @impl true
  def handle_cast({:add, %{join_ref: r} = soc}, map_) do
    {:noreply, Map.put(map_, r, soc)}
  end


  @impl true
  def handle_call({:get, key}, _from, state) do
    {:reply, Map.get(state, key), state}
  end

  @impl true
  def handle_call({:broadcast_update_from, key, data}, _from, state) do
    res = case Map.get(state, key) do
      obj ->
        UiWeb.DashboardChannel.broadcast_update_from(obj, data)
      nil ->
        {:error, "no socket with this ref"}
    end
    {:reply, res, state}
  end


  @impl true
  def handle_call(:get_all, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:remove, key}, state) do
    {:noreply, Map.delete(state, key)}
  end

  # Start the server
  def start_link(_) do
    {:ok, pid} = GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def add_socket(socket) do
    GenServer.cast(__MODULE__, {:add, socket})
  end

  def remove_socket(socket) do
    GenServer.cast(__MODULE__, {:remove, socket.join_ref})
  end

  def get_socket(ref) do
    GenServer.call(__MODULE__, {:get, ref})
  end

  def get_all() do
    GenServer.call(__MODULE__, :get_all)
  end

  def broadcast_update_from(ref, data) do
    GenServer.call(__MODULE__, {:broadcast_update_from, ref, data})
  end

  def broadcast_update_from(req, data, type) do
    case Map.get req, "joinRef" do
      nil -> {:error, "joinRef is nil"}
      ref ->
        data_ =
          data
          |> IO.inspect()
          |> Enum.map(
               fn
                 %{id: id} -> %{id: id, type: type};
                 id -> %{id: String.to_integer(id), type: type}
               end
             )
          |> IO.inspect()
        GenServer.call(__MODULE__, {:broadcast_update_from, ref, data_})
    end
  end


end
