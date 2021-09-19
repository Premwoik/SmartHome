defmodule Core.Actions.AutoLights do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Controller
  alias DB.Data.Action
  alias DB.Data.Port

  @impl true
  def init_state() do
    nil
  end

  @impl true
  def execute(_on_off, action, pid) do
    case alive?(pid) do
      true ->
        send(pid, :notified)
        :ok

      false ->
        pid = turn_on_lights(action)
        {:ok, pid}
    end
  end

  #  Privates

  defp alive?(nil), do: false
  defp alive?(pid), do: Process.alive?(pid)

  defp turn_on_lights(action) do
    ports = Action.get_ports(action, "lights")

    if length(ports) > 0 and !Port.any_on?(ports) do
      %{ok: passed} = Controller.turn_on(ports)
      time = Map.get(action.attributes, "on_timeout", 10_000)
      {:ok, pid} = Task.start(fn -> turn_off_after(passed, time) end)
      pid
    end
  end

  defp turn_off_after(ports, time) do
    receive do
      :notified ->
        Logger.info("Turning lights delayed")
        turn_off_after(ports, time)
    after
      time ->
        Logger.info("Lights turned off")
        Controller.turn_off(ports)
    end
  end
end
