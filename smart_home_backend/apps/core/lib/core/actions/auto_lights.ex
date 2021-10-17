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
  def execute(:up, action, pid) do
    case alive?(pid) do
      true ->
        send(pid, :notified)
        :ok

      false ->
        turn_on_lights(action)
    end
  end

  def execute(:down, _action, _pid) do
    :ok
  end

  #  Privates

  defp alive?(nil), do: false
  defp alive?(pid), do: Process.alive?(pid)

  defp turn_on_lights(action) do
    ports = Action.get_ports(action, "lights")

    with true <- all_lights_off(ports),
         {:ok, duration} <- Map.fetch(action.attributes, "duration"),
         %{ok: passed} = Controller.turn_on(ports) do
      Task.start(fn -> turn_off_after(passed, duration) end)
    else
      _ ->
        {:ok, nil}
    end
  end

  @spec all_lights_off([Port.t()]) :: boolean()
  defp all_lights_off(ports) do
    length(ports) > 0 and !Port.any_on?(ports)
  end

  defp turn_off_after(ports, duration) do
    receive do
      :notified ->
        Logger.info("Delay turning off lights [ids=#{get_ids(ports)}]")
        turn_off_after(ports, duration)
    after
      duration ->
        Logger.info("Turn off lights [ids=#{get_ids(ports)}]")
        Controller.turn_off(ports)
    end
  end

  defp get_ids(ports) do
    Enum.map(ports, & &1.id) |> Enum.join(", ")
  end
end
