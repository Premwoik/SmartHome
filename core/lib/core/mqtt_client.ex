defmodule Core.MqttClient do
  @moduledoc false
  use Tortoise.Handler
  alias DB.{RfButton}
  #  require Record
  #  Record.defrecord(:storage, )
  def init(args) do
    {:ok, %{}}
  end

  def connection(status, state) do
    # `status` will be either `:up` or `:down`; you can use this to
    # inform the rest of your system if the connection is currently
    # open or closed; tortoise should be busy reconnecting if you get
    # a `:down`
    {:ok, state}
  end

  def handle_message(["tele", _, "RESULT"], payload, state) do
    # unhandled message! You will crash if you subscribe to something
    # and you don't have a 'catch all' matcher; crashing on unexpected
    # messages could be a strategy though.
    %{
      "RfReceived" => %{
        "Data" => key_value
      }
    } = Poison.decode!(payload)
    btn = RfButton.get_or_create(key_value)
    cond do
      !is_nil(btn.port_id) ->
        DB.Port.get_child_struct(btn.port)
      !is_nil(btn.action_id) ->
        btn.action
      !is_nil(btn.task_id) ->
        btn.task
      true -> nil
    end
    |> Core.Controllers.UniversalController.toggle()
    {:ok, state}
  end

  def handle_message(topic, payload, state) do
    # unhandled message! You will crash if you subscribe to something
    # and you don't have a 'catch all' matcher; crashing on unexpected
    # messages could be a strategy though.
    Log
    {:ok, state}
  end

  def subscription(status, topic_filter, state) do
    {:ok, state}
  end

  def terminate(reason, state) do
    # tortoise doesn't care about what you return from terminate/2,
    # that is in alignment with other behaviours that implement a
    # terminate-callback
    :ok
  end
end
