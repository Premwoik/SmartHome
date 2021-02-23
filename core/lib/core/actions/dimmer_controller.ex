defmodule Core.Actions.DimmerController do
  @moduledoc """
    Collect ticks to set dimmer brightness.
  """
  require Logger
  alias Core.Controllers.DimmerController
  @behaviour Core.Actions.Action
  alias DB.{Port, Action}

  @impl true
  def init_state() do
    nil
  end

  @impl true
  def execute(_on_off, action, pid) do
    if alive?(pid) do
      send(pid, :notified)
      {:ok, pid}
    else
      {:ok, pid} = Task.start(fn -> delayed_command_executor(action, 1) end)
      {:ok, pid}
    end
  end

  #  Privates

  defp alive?(nil), do: false
  defp alive?(pid), do: Process.alive?(pid)

  defp delayed_command_executor(action, counter) do
    receive do
      :notified ->
        Logger.info("Action execution delayed!")
        delayed_command_executor(action, counter + 1)
    after
      5_000 ->
        Logger.info("Executing delayed action!")

        Action.arguments(action, :up)
        |> Enum.each(fn dimmer ->
          dimmer = Port.cast(dimmer, more: [fill: 0])
          fill = if counter > 3, do: 100, else: 25 * counter
          DimmerController.set_brightness(dimmer, fill)
        end)
    end
  end
end
