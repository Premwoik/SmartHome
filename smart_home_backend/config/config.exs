# This file is responsible for configuring your umbrella
# and **all applications** and their dependencies with the
# help of the Config module.
#
# Note that all applications in your umbrella share the
# same configuration and dependencies, which is why they
# all use the same configuration file. If you want different
# configurations or dependencies per app, it is best to
# move said applications out of the umbrella.
import Config

server_ip = "192.168.2.100"

config :core, two_way_client: Core.Device.Client.TwoWay
config :core, one_way_client: Core.Device.Client.OneWay

config :core, actions: Core.Actions
config :core, device_helper: Core.Device

config :core, mqtt_ip: server_ip

config :core, database_module: DB

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

config :core, Core.Scheduler,
  timezone: "Europe/Warsaw",
  run_strategy: Quantum.RunStrategy.Local

config :db,
  ecto_repos: [DB.MainRepo, DB.StatsRepo]

config :db, DB.StatsRepo,
  adapter: Ecto.Adapters.Postgres,
  database: "smart_home_statistics",
  username: "postgres",
  password: "postgres",
  hostname: server_ip,
  port: 5433

config :db, DB.MainRepo,
  adapter: Ecto.Adapters.Postgres,
  database: "smart_home",
  username: "postgres",
  password: "postgres",
  hostname: server_ip,
  port: 5433

# TODO add PubSub deps to the core
# config :core, :broadcast_handler, HomeUiWeb.Channels.BroadcastHandler

# config :home_ui,
# ecto_repos: [HomeUi.Repo]

# Configures the endpoint
config :home_ui, HomeUiWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "rQRP/j3RkvaPk6NcMaNmAifMzy19/BRrAJHZObDJMHAzxzfpVv6dJsI3Mjw07LUl",
  render_errors: [view: HomeUiWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: HomeUi.PubSub,
  live_view: [signing_salt: "tail8oyy"]

# Configures Elixir's Logger
#
config :logger,
  level: :info

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :kaffy,
  otp_app: :db,
  hide_dashboard: true,
  ecto_repo: DB.MainRepo,
  router: HomeUiWeb.Router

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
import_config "#{Mix.env()}.exs"
