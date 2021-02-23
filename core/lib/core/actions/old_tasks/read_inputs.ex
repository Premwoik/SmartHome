defmodule Core.Actions.ReadInputs do
  @moduledoc false

  alias Core.Controllers.DeviceController, as: DeviceC
  #  alias Core.Broadcast, as: Channel
  require Logger
  alias DB.Action
  alias Core.Device.Static.Response

  @behaviour Core.Actions.Action
  @actions Core.Actions

  @impl true
  def execute(_on_off, action, state) do
    #    TODO change to the device controller, that will be responsible for handling group device IO
    device = Action.arguments(action) |> List.first()

    with %Response{error: [], ok: read} <- DeviceC.read_inputs(device),
         {:ok, state} <- handle_inputs(device, read, state) do
      {:ok, state}
    else
      e ->
        Logger.error("[Task|ReadInputs - #{inspect(e)}")
        {:ok, state}
    end
  end

  @impl true
  def init_state() do
    %{last_inputs: []}
  end

  def handle_inputs(device, read, %{last_inputs: last_read} = state) do
    #    Channel.broadcast_inputs_change(device.id, read)
    new_up = read -- last_read
    new_down = last_read -- read
    proceed_up(device.id, new_up)
    proceed_down(device.id, new_down)
    log(new_up, new_down)
    {:ok, %{state | last_inputs: read}}
  end

  ## Privates

  @spec proceed_up(String.t(), list) :: any
  defp proceed_up(_, []), do: :ok

  defp proceed_up(_, read) do
    Action.get_by_activator(read)
    |> @actions.activate_up()
  end

  @spec proceed_down(String.t(), list) :: any
  defp proceed_down(_, []), do: :ok

  defp proceed_down(_, read) do
    Action.get_by_activator(read)
    |> @actions.activate_down()
  end

  defp log(up, down) do
    up = Enum.map(up, & &1.name)
    down = Enum.map(down, & &1.name)

    if length(up) > 0 || length(down) > 0,
      do:
        Logger.info(
          "Active outputs - up: #{inspect(up, charlists: :as_lists)}, down: #{
            inspect(down, charlists: :as_lists)
          }",
          ansi_color: :yellow
        )
  end
end
