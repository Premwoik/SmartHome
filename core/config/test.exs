import Config

# config :logger, level: :info

# Mocks
config :core, one_way_client: Core.Device.ClientMock
config :core, two_way_client: Core.Device.ClientMock

config :core, time_adapter: Core.Utils.Time.Mock
config :core, date_time_adapter: Core.Utils.DateTime.Mock

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase
