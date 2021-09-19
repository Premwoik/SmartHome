defmodule Core.Mqtt.Supervisor do
  use Supervisor

  alias DB.Data.Device

  @mqtt_ip Application.get_env(:core, :mqtt_ip, "localhost")

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
            host: @mqtt_ip, port: 1883
          },
          #          handler: {Tortoise.Handler.Logger, []},
          handler: {Core.MqttClient, []},
          subscriptions:
            list_of_sonoff_shellies() ++
              list_of_sonoff_basic() ++ [{"tele/sonoff-rf-bridge/RESULT", 0}]
        ]
      }
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def list_of_sonoff_basic() do
    Device.get_by_type("SonoffBasic")
    |> Enum.map(fn d -> {"stat/sonoff_basic/#{d.name}/RESULT", 0} end)
  end

  def list_of_sonoff_shellies() do
    Device.get_by_type("Shelly")
    |> Enum.map(fn d -> {"shellies/#{d.name}/relay/0", 0} end)
  end
end
