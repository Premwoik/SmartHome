defmodule Core.Tasks.ReadOutputs do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias DB.{Port}
  require Logger

  alias Core.Controllers.DeviceController, as: DeviceC
  alias Core.Broadcast, as: Channel

  @impl true
  def execute(%DB.Task{device: device}, status) do
    with {:ok, data} <- DeviceC.read_outputs(device) do
      handle_outputs(device, data, status)
    else
      e ->
        Logger.error("[Task|ReadInputs - #{inspect(e)}")
        {:ok, status}
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
    {a, res} = Port.update_low_high(device, high, low)

    if a == 0 do
      Logger.debug(
        "[Task|ReadOutputs] All outputs are up to date! device: #{device.name}, id: #{device.id}"
      )
    else
      Enum.each(res, fn {type, id, ref} -> Channel.broadcast_item_change(type, id, ref) end)

      Logger.info(
        "Updated '#{a}' ports belonging to the device with id: '#{device.id}', name: '#{
          device.name
        }'"
      )
    end
  end
end
