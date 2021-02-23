# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

import_config "../../core/config/config.exs"
import_config "../../db/config/config.exs"
import_config "../../rstp_to_ws/config/config.exs"

# Configures broadcast for core

config :core, :broadcast_handler, UiWeb.Broadcast.Handler

# Configures the endpoint
config :ui,
       UiWeb.Endpoint,
       url: [
         host: "localhost",
         port: 3000
       ],
       secret_key_base: "t+MOWbW7c8396vYNE6cqGM7pRsEgJf/vKo7RrIuDqh0Jg6KbEl6gvpWA9GpqxVDo",
       render_errors: [
         view: UiWeb.ErrorView,
         accepts: ~w(html json)
       ],
       pubsub: [
         name: Ui.PubSub,
         adapter: Phoenix.PubSub.PG2
       ]

# tell logger to load a LoggerFileBackend processes
config :logger,
  backends: [
    RingLogger,
    {LoggerFileBackend, :error_log}
    #    {LoggerFileBackend, :info_log},
    #    {LoggerFileBackend, :debug_log}
  ]

# configuration for the {LoggerFileBackend, :error_log} backend

log_path = "../logs/"
# "$time $metadata[$level] $message\n"
default_format = {Ui.LogFormatter, :format}
default_metadata = [:request_id, :application]

# Set the number of messages to hold in the circular buffer
config :logger, RingLogger,
  max_size: 1024,
  format: default_format,
  metadata: default_metadata

# You can also configure RingLogger.Client options to be used
# with every client by default
config :ring_logger,
  application_levels: %{my_app: :error},
  color: [debug: :yellow],
  level: :debug

config :logger,
       :error_log,
       path: log_path <> "error.log",
       format: default_format,
       metadata: default_metadata,
       level: :error

config :logger,
       :info_log,
       path: log_path <> "info.log",
       format: default_format,
       metadata: default_metadata,
       level: :info

config :logger,
       :debug_log,
       path: log_path <> "debug.log",
       format: default_format,
       metadata: default_metadata,
       level: :debug

# Configures Elixir's Logger
config :logger,
       :console,
       format: default_format,
       metadata: default_metadata

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
