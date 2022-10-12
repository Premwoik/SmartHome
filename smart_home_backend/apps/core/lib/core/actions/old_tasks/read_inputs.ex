defmodule Core.Actions.ReadInputs do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Broadcast, as: Channel
  alias Core.DeviceController
  alias Core.Device.Static.Response
  alias DB.Data.Device
  alias DB.Proc.ActionListProc

  @impl true
  def execute(_on_off, action, state) do
    with {:ok, device} <- action.attributes["device_id"] |> Device.find(),
         %Response{error: [], ok: read} <- DeviceController.read_inputs(device),
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
    Channel.broadcast_inputs_change(device.id, read)
    new_up = read -- last_read
    new_down = last_read -- read
    proceed_up(device.id, new_up)
    proceed_down(device.id, new_down)
    log(new_up, new_down)
    {:ok, %{state | last_inputs: read}}
  end

  ### Privates

  @spec proceed_up(String.t(), list) :: any
  defp proceed_up(_, []), do: :ok

  defp proceed_up(_, read) do
    case ActionListProc.get_by_activator(read) do
      {:ok, actions} ->
        actions
        |> Enum.map(& &1.id)
        |> Core.Actions.activate_up()

      {:error, :not_found} ->
        :ok
    end
  end

  @spec proceed_down(String.t(), list) :: any
  defp proceed_down(_, []), do: :ok

  defp proceed_down(_, read) do
    case ActionListProc.get_by_activator(read) do
      {:ok, actions} ->
        actions
        |> Enum.map(& &1.id)
        |> Core.Actions.activate_down()

      {:error, :not_found} ->
        :ok
    end
  end

  defp log(up, down) do
    if length(up) > 0 || length(down) > 0 do
      # Write input change to database
      write(up, down)

      opts = [charlists: :as_lists]
      up = Enum.map(up, & &1.name)
      down = Enum.map(down, & &1.name)

      Logger.info(
        "Active outputs - up: #{inspect(up, opts)}, down: #{inspect(down, opts)}",
        ansi_color: :yellow,
        write_log: false
      )
    end
  end

  alias DB.Series.Input
  alias DB.InfluxConnection

  defp write(up, down) do
    up = Enum.map(up, &Input.new(&1.device_id, &1.number, 1))
    down = Enum.map(down, &Input.new(&1.device_id, &1.number, 0))
    InfluxConnection.write(up ++ down)
  end
end
