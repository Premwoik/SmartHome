defmodule Core.Actions do
  @moduledoc false

  @callback activate_up(ids :: list(integer), name :: atom) :: any
  @callback activate_down(ids :: list(integer), name :: atom) :: any

  @behaviour Core.Actions

  use GenServer
  use Timex
  require Logger

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

  def get_state(name \\ __MODULE__) do
    try do
      GenServer.call(name, :get_state)
    catch
      :exit, _ -> :ok
    end
  end

  def reload_actions(name \\ __MODULE__) do
    try do
      GenServer.call(name, :reload_actions)
    catch
      :exit, _ -> :ok
    end
  end

  ## Callbacks

  @impl true
  def init(_) do
    {:ok, %{actions: DB.Action.all_active(), amem: %{}}}
  end

  @impl true
  def handle_call({:activate, up_down, ids}, _from, %{amem: amem} = s) do
    results = invoke_actions(up_down, ids, s)
    {amem_, errors} = after_invoke(results, amem)
    {:reply, {:ok, errors}, %{s | amem: amem_}}
  end

  @impl true
  def handle_call(:get_state, _from, s) do
    {:reply, s, s}
  end

  @impl true
  def handle_call(:reload_actions, _from, %{amem: amem} = s) do
    actions = DB.Action.all_active()
    amem_ = :maps.filter(fn id, _ -> Enum.any?(actions, fn %{id: id_} -> id_ == id end) end, amem)
    {:reply, :ok, %{s | actions: actions, amem: amem_}}
  end

  #  Privates
  defp after_invoke(result, amem, errors \\ [])
  defp after_invoke([], amem, errors) do
    {amem, errors}
  end

  defp after_invoke([r | results], amem, errors) do
    case r do
      {:ok, {id, mem}} ->
        amem_ = Map.put(amem, id, mem)
        after_invoke(results, amem_, errors)

      {:error, err} ->
        after_invoke(results, amem, [err | errors])
    end
  end

  @spec invoke_actions(atom, list(integer), map) :: map
  defp invoke_actions(on_off, ids, %{actions: actions, amem: amem} = s) do
    match? = fn id, action -> action.id == id end

    for id <- ids,
        action <- actions,
        match?.(id, action) do
      proceed_action(on_off, action, amem[action.id])
    end
  end

  @spec proceed_action(Core.Actions.Action.state(), map, map) :: map
  defp proceed_action(on_off, action, nil) do
    amem = get_module(action.function).init_memory()
    proceed_action(on_off, action, amem)
  end

  defp proceed_action(on_off, action, amem) do
    with {:ok, amem_} <- check_activation_freq(action.frequency, amem),
         :ok <- check_activation_time(action.start_time, action.end_time) do
      try do
        result = apply(get_module(action.function), :execute, [on_off, action, amem_])
        {:ok, {action.id, result}}
      rescue
        e in RuntimeError -> {:error, {action.id, e}}
      end
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

  def get_module(function) do
    String.to_existing_atom("Elixir.Core.Actions." <> function)
  end
end
