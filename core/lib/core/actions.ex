defmodule Core.Actions do
  @moduledoc false

  use GenServer
  alias DB.Dao
  alias Device.Controller
  alias Device.Command
  require Logger

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  ## Public

  def reload, do:
    Process.exit(__MODULE__, :normal)

  def invoke_up_actions(dname, ports, name \\ __MODULE__) do
    GenServer.cast name, {:up_action, ports, dname}
  end

  def invoke_down_actions(dname, ports, name \\ __MODULE__) do
    GenServer.cast name, {:down_action, ports, dname}
  end


  def reload_actions(name \\ __MODULE__), do:
    GenServer.cast name, :reload_actions


  ## Callbacks

  @impl true
  def init(_) do
    {:ok, %{actions: Dao.find_actions(), amem: %{}}}
  end

  @impl true
  def handle_cast({:up_action, actionList, dname}, s) do
    ns = handle_action s, actionList, dname, :on
    {:noreply, ns}
  end

  @impl true
  def handle_cast({:down_action, actionList, dname}, s) do
    ns = handle_action s, actionList, dname, :off
    {:noreply, ns}
  end

  @impl true
  def handle_cast(:reload_actions, %{amem: amem} = s) do
    actions = Dao.find_actions()
    actionIds = Keyword.values actions
                               |> Enum.map fn {id, _} -> id end
    newAmem = :maps.filter fn id, _ -> Enum.any? actionIds &(&1 == id)  end
    {:noreply, %{s | actions: actions, amem: newAmem}}
  end

  #  Privates
  defp handle_action(s, [], _, _), do: s
  defp handle_action(%{actions: actions, amem: amem} = s, [h | t], dname, onOff) do
    try do
      (case List.keyfind(actions, {(to_string dname), h}, 0)do
         :nil ->
           s
         #        update action memory if action proceeded
         {_, {actionId, fun}} ->
           action = %{}
           newMem = proceed_action onOff, action, amem[actionId]
           %{s | amem: (Map.put amem, actionId, newMem)}
       end)
      |> handle_action(t, dname, onOff)
    rescue
      error ->
        IO.inspect error
        Logger.error("Handle action error")
        handle_action t, dname, onOff, s
    end
  end

  @spec proceed_action(Core.Actions.Action.state, map, map) :: map
  defp proceed_action(onOff, action, nil) do
    amem = apply get_module(action.function), :init_memory, []
    proceed_action(onOff, action, amem)
  end
  defp proceed_action(onOff, action, amem) do
    with {:ok, amem_} <- check_activation_delay(action.delay, amem),
         :ok <- check_activation_time(action.time_start, action.time_end)
      do
      apply get_module(action.function), :execute, [onOff, action, amem_]
    else
      amem
    end
  end


  @spec check_activation_delay(integer, map) :: {:ok, map} | :fail
  defp check_activation_delay(0, amem), do: {:ok, amem}
  defp check_activation_delay(_, nil), do:
    {:ok, %{lastInvoke: :os.system_time(:millisecond)}}
  defp check_activation_delay(delay, %{lastInvoke} = amem) do
    currentTime = :os.system_time(:millisecond)
    if currentTime - lastInvoke > delay do
      {:ok, %{amem | lastInvoke: currentTime}}
    else
      :fail
    end
  end
  defp check_activation_delay(_, amem) do
    {:ok, Map.put amem, :lastInvoke, :os.system_time(:millisecond)}
  end

  @spec check_activation_time(Time.t, Time.t) :: :ok | :fail
  defp check_activation_time(nil, nil), do: :ok
  defp check_activation_time(times, timee) do
    now = Time.utc_now()
    if Time.compare(now, times) == :gt && Time.compare(now, timee) == :lt do
      :ok
    else
      :fail
    end
  end


  def get_module(function) do
    String.to_atom "Elixir.Actions." <> function
  end

end
