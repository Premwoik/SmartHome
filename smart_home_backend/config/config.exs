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

config :core, :target, System.get_env("TARGET", "rpi")

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
config :ui, UiWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: UiWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Ui.PubSub,
  live_view: [signing_salt: "gxuEv+rN"]

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :ui, Ui.Mailer, adapter: Swoosh.Adapters.Local

# Swoosh API client is needed for adapters other than SMTP.
config :swoosh, :api_client, false

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.14.0",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../apps/ui/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configures Elixir's Logger
#
config :logger,
  level: :info

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :logger,
  backends: [{Core.LoggerHistoryBackend, :logger_cache}]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
import_config "#{Mix.env()}.exs"
