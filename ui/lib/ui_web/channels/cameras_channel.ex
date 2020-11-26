defmodule UiWeb.CamerasChannel do
  use UiWeb, :channel
  alias RstpToWs.Camera.Converter

  require Logger


  @impl true
  def join("cameras:lobby", payload, socket) do
    if authorized?(payload) do
#      Converter.register_consumer()
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client

  @impl true
  def handle_in("register", payload, socket) do
    Logger.debug("Registering of a camera stream consumer.")
    Converter.register_consumer(socket.join_ref)
    {:reply, {:ok, payload}, socket}
  end

  @impl true
  def handle_in("unregister", payload, socket) do
    Logger.debug("Unregistering of a camera stream consumer.")
    Converter.unregister_consumer(socket.join_ref)
    {:reply, {:ok, payload}, socket}
  end



  @impl true
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (cameras_channel:lobby).
  @impl true
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  @impl true
  def terminate(reason, socket) do
    Converter.unregister_consumer(socket.join_ref)
    Logger.debug("Client #{inspect socket} leave with reason #{inspect reason}")
#    Converter.unregister_consumer()
  end

  # Add authorization logic here as required.

  defp authorized?(_payload) do
    true
  end
end
