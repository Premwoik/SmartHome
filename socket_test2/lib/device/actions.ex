defmodule Alarm.Actions do
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
    ns = handle_action actionList, dname, :on, s
    {:noreply, ns}
  end

  @impl true
  def handle_cast({:down_action, actionList, dname}, s) do
    ns = handle_action actionList, dname, :off, s
    {:noreply, ns}
  end



  ## Privates
  defp handle_action([], _, _, s), do: s
  defp handle_action([h | t], dname, onOff, %{actions: actions, amem: actionMems} = s) do
    try do
      newActionMem = case List.keyfind(actions, {(to_string dname), h}, 0)do
        :nil ->
          s
        #        update action memory if action proceeded
        {_, {actionId, fun}} ->
          newMem = apply __MODULE__, (String.to_atom fun), [onOff, actionId, actionMems[actionId]]
          %{s | amem: (Map.put actionMems, actionId, newMem)}
      end
      handle_action t, dname, onOff, newActionMem
    rescue
      error ->
        #        Logger.error("Wrong function name")
        IO.inspect error
        Logger.error("Handle action error")
        handle_action t, dname, onOff, s
    end
  end

  ## Functions

  def close_sunblinds(onOff, action_id, s) do
    Logger.info("CloseSunblinds action is invoked.")
    state = case onOff,
                 do: (
                   :on -> true;
                   :off -> false)
    #TODO add sort of filtration e.x. groups?
    #if there was no need to close all sunblinds
    # or maybe close all, but open only several e.x special group to open and special group to close
    data = Dao.get_ports_by_type("sunblind")
           |> Enum.group_by(&(&1.device_id))
           |> Map.to_list()
    for {deviceId, sunblinds} <- data, do:
      Controller.set_outputs (Dao.get_device_atom deviceId), sunblinds, state
    s
  end


  def turn_lights_on(onOff, actionId, nil), do:
    #  init memory
    turn_lights_on(onOff, actionId, %{lastInvoke: 0, offPid: nil})
  def turn_lights_on(onOff, actionId, %{lastInvoke: lastInvoke, offPid: offPid} = s) do
    interval = 15_000
    currentTime = :os.system_time(:millisecond)
    if currentTime - lastInvoke > interval do
      %{s | lastInvoke: currentTime, offPid: (notify_off_task offPid, actionId)}
    else
      s
    end
  end

  defp notify_off_task(nil, actionId) do
    action = Dao.find_action(actionId)
    if isAnyOn? action.args do
      nil
    else
      [timeout|_] = Poison.decode! action.params
      Controller.set_dim_lights(action.args, true)
      {ok, pid} = Task.start __MODULE__, :turn_lights_off, [timeout, action.args]
      pid
    end
  end
  defp notify_off_task(pid, ops) do
    case Process.alive? pid do
      true ->
        send pid, :notified
        pid
      false ->
        notify_off_task nil, ops
    end
  end

  defp isAnyOn?(ports), do:
      Enum.any? ports, &(&1.state == true)

  def turn_lights_off(timeout, lights) do
    receive do
      :notified ->
        Logger.info("Turning lights deleyed")
        turn_lights_off(timeout, lights)
    after
      timeout ->
        Logger.info("Lights turned off")
        Controller.set_dim_lights(lights, false)
    end
  end


end
