defmodule Core.Actions do
  @moduledoc false

  #  @callback activate_up(ids :: list(integer), name :: atom) :: any
  #  @callback activate_down(ids :: list(integer), name :: atom) :: any

  use GenServer
  use Timex
  require Logger
  alias DB.Action

  @type memory() :: map()
  @type memories() :: map()
  @type state() :: any()

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  ## Public

  def reload,
    do:
      __MODULE__
      |> Process.whereis()
      |> Process.exit(:normal)

  def activate_up(ids, name \\ __MODULE__) do
    try do
      GenServer.call(name, {:activate, :up, ids})
    catch
      :exit, _ -> :ok
    end
  end

  def activate_down(ids, name \\ __MODULE__) do
    try do
      GenServer.call(name, {:activate, :down, ids})
    catch
      :exit, _ -> :ok
    end
  end

  def reload_actions(_name \\ __MODULE__) do
    :ok
    #    try do
    #      GenServer.call(name, :reload_actions)
    #    catch
    #      :exit, _ -> :ok
    #    end
  end

  ## Callbacks

  @impl true
  def init(_) do
    {:ok, %{amem: %{}}}
  end

  @impl true
  def handle_call({:activate, up_down, ids}, _from, s) do
    errors =
      invoke_actions(up_down, ids, s)
      |> filter_errors()

    {:reply, {:ok, errors}, s}
  end

  @impl true
  def handle_cast({:action_result, id, state}, %{amem: memories} = s) do
    memories = Map.put(memories, id, state)
    {:noreply, %{s | amem: memories}}
  end

  ##   Privates

  defp filter_errors(results) do
    Enum.filter(results, fn x ->
      case x do
        :ok -> false
        _ -> true
      end
    end)
  end

  @spec invoke_actions(atom, list(integer), map) :: map
  defp invoke_actions(on_off, ids, state) do
    for action <- Action.get_active(ids) do
      Logger.debug("[Action|#{action.name}] Invoking action in mode [#{inspect(on_off)}]")
      proceed_action(on_off, action, get_memory(action, state))
    end
  end

  def get_memory(action, %{amem: memes}) do
    with nil <- Map.get(memes, action.id) do
      %{state: get_module(action.function).init_state()}
    end
  end

  @spec proceed_action(Core.Actions.Action.state(), %DB.Action{}, map) :: map
  defp proceed_action(on_off, action, memory) do
    with {:ok, memory} <- check_activation_freq(action.timeout, memory),
         :ok <- check_activation_time(action.start_time, action.end_time) do
      spawn_link(fn -> run_action(self(), on_off, action, memory) end)
      :ok
    else
      {:error, err} -> {:error, {action.id, err}}
    end
  end

  @spec check_activation_freq(integer, map) :: {:ok, map} | :fail
  defp check_activation_freq(0, amem), do: {:ok, amem}

  defp check_activation_freq(delay, %{lastInvoke: last_invoke} = amem) do
    current_time = :os.system_time(:millisecond)

    if current_time - last_invoke > delay do
      {:ok, %{amem | lastInvoke: current_time}}
    else
      {:error, "Its too early to invoke action again"}
    end
  end

  defp check_activation_freq(_, amem) do
    {:ok, Map.put(amem, :lastInvoke, :os.system_time(:millisecond))}
  end

  @spec check_activation_time(Time.t(), Time.t()) :: :ok | :fail
  defp check_activation_time(nil, nil), do: :ok

  defp check_activation_time(times, timee) do
    if Core.Utils.Time.in_interval?(times, timee) do
      :ok
    else
      {:error, "Its too early or too late to invoke this action"}
    end
  end

  @spec run_action(pid, Core.Actions.Action.state(), %DB.Action{}, state()) :: any
  defp run_action(server_pid, on_off, action, memory) do
    module = get_module(action.function)

    try do
      memory =
        case apply(module, :execute, [on_off, action, memory.state]) do
          {:ok, state} -> Map.put(memory, :state, state)
          _otherwise -> memory
        end
      GenServer.cast(__MODULE__, {:action_result, action.id, memory})
    rescue
      e in RuntimeError ->
        Logger.error("[Action|#{action.name} exits with error.\n #{inspect(e)}")
    end
  end

  defp get_module(function) do
    String.to_existing_atom("Elixir.Core.Actions." <> function)
  end
end
