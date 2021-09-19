defmodule Core.MqttClient do
  @moduledoc false
  use Tortoise.Handler

  require Logger

  alias Core.Device.{SonoffBasic, Shelly}
  alias Core.Mqtt.RfButtonHandler
  alias DB.Data.RfButton

  def init(_args) do
    {:ok, %{pages: %{}}}
  end

  def connection(_status, state) do
    # `status` will be either `:up` or `:down`; you can use this to
    # inform the rest of your system if the connection is currently
    # open or closed; tortoise should be busy reconnecting if you get
    # a `:down`
    {:ok, state}
  end

  def handle_message(["shellies", name, "relay", "0"], payload, state) do
    with {:ok, state} <- Shelly.handle_mqtt_result(name, payload, state) do
      {:ok, state}
    else
      :ok ->
        {:ok, state}

      _ ->
        Logger.info("Can not find handler for device: #{name}}")
        {:ok, state}
    end
  end

  def handle_message(["stat", "sonoff_basic", name, "RESULT"], payload, state) do
    with {:ok, state} <- SonoffBasic.handle_mqtt_result(name, payload, state) do
      {:ok, state}
    else
      :ok ->
        {:ok, state}

      _ ->
        Logger.info("Can not find handler for device: #{name}}")
        {:ok, state}
    end
  end

  def handle_message(["tele", _, "RESULT"], payload, state) do
    # unhandled message! You will crash if you subscribe to something
    # and you don't have a 'catch all' matcher; crashing on unexpected
    # messages could be a strategy though.
    Logger.info("RfButton handling #{inspect(payload)}")

    %{
      "RfReceived" => %{
        "Data" => key_value
      }
    } = Poison.decode!(payload)

    btn = RfButton.identify(key_value)
    state = RfButtonHandler.handle_button_click(btn, state)
    {:ok, state}
  end

  def handle_message(topic, payload, state) do
    # unhandled message! You will crash if you subscribe to something
    # and you don't have a 'catch all' matcher; crashing on unexpected
    # messages could be a strategy though.
    Logger.error("Not handled topic: #{inspect(topic)}, with payload: #{payload}")
    {:ok, state}
  end

  def subscription(_status, _topic_filter, state) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    # tortoise doesn't care about what you return from terminate/2,
    # that is in alignment with other behaviours that implement a
    # terminate-callback
    :ok
  end
end
