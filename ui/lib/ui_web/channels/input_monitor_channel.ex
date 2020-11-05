defmodule UiWeb.InputMonitorChannel do
  use UiWeb, :channel

  def join("inputs_monitor:lobby", payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (input_monitor:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end

  """
    up - all up inputs
    down - all down inputs
  """
  @spec broadcast_inputs_state(integer(), list()) :: any()
  def broadcast_inputs_state(device_id, up) do
    data = %{
      device_id: device_id,
      numbers: up
    }
    UiWeb.Endpoint.broadcast("inputs_monitor:lobby", "active:inputs", data)
  end

end
