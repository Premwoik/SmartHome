defmodule Core.Actions.ReadOutputs do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias Core.Broadcast, as: Channel
  alias Core.Controllers.DeviceController, as: DeviceC
  alias DB.Data.Device
  alias DB.Proc.PortListProc

  @impl true
  def execute(_on_off, action, state) do
    with {:ok, device} <- Device.find(action.attributes["device_id"]),
         {:ok, data} <- DeviceC.read_outputs(device) do
      handle_outputs(device, data, state)
    else
      e ->
        Logger.error("[Task|ReadInputs - #{inspect(e)}")
        {:ok, state}
    end
  end

  @impl true
  def init_state() do
    %{last_outputs: []}
  end

  def handle_outputs(device, read, %{last_outputs: last_read} = status) do
    if read == last_read do
      Logger.debug(
        "[Task|ReadOutputs] Device #{device.name}, id: #{device.id} outputs don't changed!"
      )

      {:ok, status}
    else
      Task.start(fn -> update_out_of_date(device, read -- last_read, last_read -- read) end)
      {:ok, %{status | last_outputs: read}}
    end
  end

  # Private

  defp update_out_of_date(device, high, low) do
    high_ports =
      PortListProc.identify!(device.id, high)
      |> Enum.map(&PortListProc.update!(&1.id, %{state: true}))

    low_ports =
      PortListProc.identify!(device.id, low)
      |> Enum.map(&PortListProc.update!(&1.id, %{state: false}))

    res = high_ports ++ low_ports
    len = length(res)

    if len == 0 do
      Logger.debug(
        "[Task|ReadOutputs] All outputs are up to date! device: #{device.name}, id: #{device.id}"
      )
    else
      Enum.each(res, fn %{type: type} = item ->
        Channel.broadcast_item_change(type, item)
      end)

      Logger.info(
        "Updated '#{len}' ports belonging to the device with id: '#{device.id}', name: '#{
          device.name
        }'"
      )
    end
  end
end
