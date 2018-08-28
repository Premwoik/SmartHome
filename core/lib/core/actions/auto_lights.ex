defmodule Core.Actions.AutoLights do
  @moduledoc false

  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{lastInvoke: 0, offPid: nil}
  end


  @impl true
  def execute(on_off, action, %{offPid: pid} = amem) do
    case pid_valid? pid do
      true ->
        send pid, :notified
        amem
      false ->
       turn_on_lights(action)
       |> update_pid(amem)
    end
  end

#  Privates

  defp pid_valid?(nil), do: false
  defp pid_valid?(pid), do:
    Process.alive? pid

  defp turn_on_lights(action) do
    if any_on? action.args do
      nil
    else
      [timeout|_] = Poison.decode! action.params
      Controller.set_dim_lights(action.args, true)
      {:ok, pid} = Task.start fn -> turn_lights_off timeout, action.args end
      pid
    end
  end

  defp update_pid(nil, mem), do: mem
  defp update_pid(pid, mem) do
    Map.put mem, :offPid, pid
  end

  defp any_on?(ports), do:
      Enum.any? ports, &(&1.state == true)

  defp turn_lights_off(timeout, lights) do
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
