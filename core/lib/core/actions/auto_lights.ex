defmodule Core.Actions.AutoLights do
  @moduledoc false
  require Logger
  alias DB.Dao
  alias Core.Controllers.LightController
  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{lastInvoke: 0, offPid: nil}
  end


  @impl true
  def execute(on_off, action, %{offPid: pid} = amem) do
    case alive? pid do
      true ->
        send pid, :notified
        amem
      false ->
        turn_on_lights(action)
        |> update_pid(amem)
    end
  end

  #  Privates

  defp alive?(nil), do: false
  defp alive?(pid), do:
    Process.alive? pid

  defp turn_on_lights(action) do
    lights =
      DB.Action.get_args_ids(action)
      |> DB.Light.get()

    if any_on? lights do
      nil
    else
      case LightController.turn_on(lights) do
        :ok ->
          [time | _] = Poison.decode! action.params
          {:ok, pid} = Task.start fn -> turn_off_after(lights, time) end
          pid
        error ->
          nil
      end
    end
  end

  defp turn_off_after(lights, time) do
    receive do
      :notified ->
        Logger.info("Turning lights delayed")
        turn_off_after(lights, time)
    after
      time ->
        Logger.info("Lights turned off")
        LightController.turn_off(lights)
    end
  end

  defp update_pid(nil, mem), do: mem
  defp update_pid(pid, mem) do
    Map.put mem, :offPid, pid
  end

  defp any_on?(lights), do:
    Enum.any? lights, &(&1.port.state == true)

end
