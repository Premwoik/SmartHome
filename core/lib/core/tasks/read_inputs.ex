defmodule Core.Tasks.ReadInputs do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias Core.Controllers.BasicController, as: Basics
  alias UiWeb.InputMonitorChannel, as: Channel
  require Logger

#  @device Application.get_env(:core, :device_helper)
#  @actions Application.get_env(:core, :actions_server)

  @device Core.Device
  @actions Core.Actions

  @impl true
  def execute(task, %{last_read: last_read} = state) do

    #IO.puts "Before read inputs #{NaiveDateTime.utc_now()}"
    case Basics.read(task.device) do
      {:ok, read} ->
        Channel.broadcast_inputs_state(task.device_id, read)
        #IO.puts "After read inputs #{NaiveDateTime.utc_now()}"
        new_up = read -- last_read
        new_down = last_read -- read

        proceed_up(task.device.id, new_up)
        proceed_down(task.device.id, new_down)
        log(new_up, new_down)

        {:ok, %{state | last_read: read}}

      error ->
        Logger.error("#{task.device.name} read error: #{inspect(error)}")
        :error
    end
  end

  @impl true
  def init_state() do
    %{last_read: []}
  end

  ## Privates

  @spec proceed_up(string, list) :: any
  defp proceed_up(_, []), do: :ok

  defp proceed_up(d_id, read),
    do:
      DB.Action.get_by_activator(d_id, read)
      |> @actions.activate_up()

  @spec proceed_down(string, list) :: any
  defp proceed_down(_, []), do: :ok

  defp proceed_down(d_id, read),
    do:
      DB.Action.get_by_activator(d_id, read)
      |> @actions.activate_down()

  defp log(up, down) do
    if length(up) > 0 || length(down) > 0,
      do:
        Logger.info(
          "active outputs - up: #{inspect(up, charlists: :as_lists)}, down: #{
            inspect(down, charlists: :as_lists)
          }",
          ansi_color: :yellow
        )
  end
end
