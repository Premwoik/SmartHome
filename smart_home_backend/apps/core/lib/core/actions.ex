defmodule Core.Actions do
  @moduledoc false

  use GenServer
  use Timex

  require Logger

  alias DB.Data.Action
  alias DB.Proc.ActionListProc
  alias Core.Actions.Action, as: ActionB

  @type action_state() :: map()
  @type full_action_state() :: %{last_invoke: integer(), state: action_state()}
  @type state() :: %{actions_state: %{integer() => full_action_state()}}

  # flush inactive actions' states every hour
  @flush_interval 1 * 24 * 3_600_000

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
    rescue
      _ -> :error
    catch
      :exit, _ -> :error
    end
  end

  def activate_down(ids, name \\ __MODULE__) do
    try do
      GenServer.call(name, {:activate, :down, ids})
    rescue
      _ -> :error
    catch
      :exit, _ -> :error
    end
  end

  ## Callbacks

  @impl true
  def init(_) do
    flush_inactive_states()
    {:ok, %{actions_state: %{}}}
  end

  @impl true
  def handle_call({:activate, up_down, ids}, _from, s) do
    errors =
      invoke_actions(up_down, ids, s)
      |> filter_errors()

    {:reply, {:ok, errors}, s}
  end

  @impl true
  def handle_cast({:action_result, action_id, action_state}, state) do
    {:noreply, put_action_state(state, action_id, action_state)}
  end

  @impl true
  def handle_info(:flush_inactive_states, %{actions_state: as} = state) do
    flush_inactive_states()
    state_ids = Map.keys(as)

    with {:ok, active_actions} <- ActionListProc.list_active(),
         true <- length(active_actions) < length(state_ids) do
      active_actions_ids = Enum.map(active_actions, & &1.id)
      inactive_actions_ids = state_ids -- active_actions_ids
      flushed_as = Map.drop(as, inactive_actions_ids)
      {:noreply, %{state | actions_state: flushed_as}}
    else
      _ ->
        {:noreply, state}
    end
  end

  ##   Privates

  @spec put_action_state(state(), integer(), full_action_state()) :: state()
  defp put_action_state(server_state, action_id, action_state) do
    put_in(server_state, [:actions_state, action_id], action_state)
  end

  defp filter_errors(results) do
    Enum.filter(results, fn x ->
      case x do
        :ok -> false
        _ -> true
      end
    end)
  end

  @spec invoke_actions(atom, list(integer), state()) :: [any()]
  defp invoke_actions(on_off, ids, state) do
    {:ok, actions} = ActionListProc.list_active()

    for action <- Enum.filter(actions, &(&1.id in ids)) do
      proceed_action(on_off, action, get_memory(action, state))
    end
  end

  @spec get_memory(Action.t(), state()) :: full_action_state()
  def get_memory(action, %{actions_state: states}) do
    with nil <- Map.get(states, action.id) do
      %{state: get_module(Map.fetch!(action, :module)).init_state(), last_invoke: 0}
    end
  end

  @spec proceed_action(ActionB.state(), Action.t(), action_state()) :: :ok | {:error, tuple()}
  defp proceed_action(on_off, action, action_state) do
    with {:ok, action_state} <- check_activation_freq(action.pause, action_state),
         :ok <- check_activation_time(action.start_time, action.end_time) do
      spawn_link(fn -> run_action(self(), on_off, action, action_state) end)
      :ok
    else
      {:error, err} -> {:error, {action.id, err}}
    end
  end

  @spec check_activation_freq(integer() | nil, full_action_state()) ::
          {:ok, action_state()} | {:error, String.t()}
  defp check_activation_freq(0, action_state), do: {:ok, action_state}

  defp check_activation_freq(nil, action_state), do: {:ok, action_state}

  defp check_activation_freq(delay, %{last_invoke: last_invoke} = action_state) do
    current_time = :os.system_time(:millisecond)

    if current_time - last_invoke > delay do
      {:ok, %{action_state | last_invoke: current_time}}
    else
      {:error, "Its too early to invoke action again"}
    end
  end

  @spec check_activation_time(nil | Time.t(), nil | Time.t()) :: :ok | {:error, String.t()}
  defp check_activation_time(nil, nil), do: :ok

  defp check_activation_time(t_start, t_end) do
    if Core.Utils.Time.in_interval?(t_start, t_end) do
      :ok
    else
      {:error, "Its too early or too late to invoke this action"}
    end
  end

  @spec run_action(pid(), ActionB.state(), atom() | Action.t(), full_action_state()) :: any
  defp run_action(_server_pid, on_off, action, memory) do
    module = get_module(action.module)

    try do
      {duration, result} =
        Benchmark.measure_r(fn -> apply(module, :execute, [on_off, action, memory.state]) end)

      memory =
        case result do
          {:ok, state} -> Map.put(memory, :state, state)
          _otherwise -> memory
        end

      :ok =
        :telemetry.execute(
          [:core, :action, :run],
          %{duration: duration},
          %{action: action, mode: on_off}
        )

      GenServer.cast(__MODULE__, {:action_result, action.id, memory})
    rescue
      e in RuntimeError ->
        Logger.error("[Action|#{action.name} exits with error.\n #{inspect(e)}")
    end
  end

  defp get_module(function) do
    String.to_existing_atom("Elixir.Core.Actions." <> function)
  end

  defp flush_inactive_states(),
    do: Process.send_after(self(), :flush_inactive_states, @flush_interval)
end
