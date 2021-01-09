defmodule UiWeb.Broadcast.Handler do
  @moduledoc false

  use Core.Broadcast

  # IOMonitor

  @impl true
  @spec broadcast_inputs_change(integer(), list()) :: any()
  def broadcast_inputs_change(device_id, up) do
    data = %{
      device_id: device_id,
      numbers: up
    }

    UiWeb.Endpoint.broadcast("inputs_monitor:lobby", "active:inputs", data)
  end

  @impl true
  @spec broadcast_outputs_change(integer(), list()) :: any()
  def broadcast_outputs_change(device_id, up) do
    data = %{
      device_id: device_id,
      numbers: up
    }

    UiWeb.Endpoint.broadcast("inputs_monitor:lobby", "active:outputs", data)
  end

  # ItemChange

  @impl true
  def broadcast_item_change(type, id, ref) do
    data = %{
      type: type,
      id: id,
      ref: ref
    }

    UiWeb.Endpoint.broadcast("dashboard:lobby", "object:updated", data)
  end
end
