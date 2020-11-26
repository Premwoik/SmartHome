defmodule Core.Actions.DimmerController do
  @moduledoc false
  require Logger
  alias DB.Dao
  alias Core.Controllers.LightController
  alias Core.Controllers.DimmerController
  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{pid: nil}
  end

  @impl true
  def execute(on_off, action, %{pid: pid}) do
    if alive?(pid) do
      send(pid, :notified)
      %{pid: pid}
    else
      {:ok, pid} = Task.start(fn -> delayed_command_executor(action, 1) end)
      %{pid: pid}
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
        args = DB.Action.get_args_ids(action.id)
        DB.Dimmer.get_by_port(args)
        |> Enum.each(
             fn dimmer ->
               dimmer_ = %{dimmer | fill: 0, lights: []}
               fill = if counter > 3, do: 100, else: 25 * counter
               DimmerController.set_brightness(dimmer_, fill)
             end
           )
    end
  end
end
