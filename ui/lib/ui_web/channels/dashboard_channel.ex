defmodule UiWeb.DashboardChannel do
  use UiWeb, :channel

  def join("dashboard:lobby", payload, socket) do
#    if authorized?(payload) do
      send(self, :after_join)
      {:ok, socket}
#    else
#      {:error, %{reason: "unauthorized"}}
#    end
  end

  def terminate(reason, socket) do
    UiWeb.DashboardChannel.Helper.remove_socket(socket)
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (dashboard:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    UiWeb.DashboardChannel.Helper.add_socket(%{socket | joined: true})
    {:noreply, socket}
  end


  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end

  def broadcast_update_from(socket, changes) do
    broadcast_from socket, "update", %{data: changes}
  end

  def broadcast_update(socket, changes) do
    broadcast socket, "update", %{data: changes}
  end

end
