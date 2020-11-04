defmodule Core.Mqtt.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      {
        Tortoise.Connection,
        [
          client_id: Core.MqttClient,
          server: {
            Tortoise.Transport.Tcp,
            host: '192.168.2.100',
            port: 1883
          },
#          handler: {Tortoise.Handler.Logger, []},
          handler: {Core.MqttClient, []},
          subscriptions: [{"tele/sonoff-rf-bridge/RESULT", 0}]
        ]
      }
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
