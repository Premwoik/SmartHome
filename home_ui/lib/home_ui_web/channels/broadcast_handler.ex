defmodule HomeUiWeb.Channels.BroadcastHandler do
  @moduledoc false

  use Core.Broadcast
  import HomeUiWeb.Endpoint

  # IOMonitor

  @impl true
  @spec broadcast_inputs_change(integer(), list()) :: any()
  def broadcast_inputs_change(device_id, up) do
    data = %{
      device_id: device_id,
      numbers: up
    }

    broadcast("monitor:lobby", "active:inputs", data)
  end

  @impl true
  @spec broadcast_outputs_change(integer(), list()) :: any()
  def broadcast_outputs_change(device_id, up) do
    data = %{
      device_id: device_id,
      numbers: up
    }

    broadcast("monitor:lobby", "active:outputs", data)
  end

  # ItemChange

  @impl true
  def broadcast_item_change(type, item) do
    data = %{
      type: type,
      item: item
    }

    broadcast("dashboard:lobby", "object:updated", data)
  end
end
