# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

#import_config "../../db/config/config.exs"
# Configures the endpoint
config :ui, UiWeb.Endpoint,
  url: [host: "192.168.2.119"],
  secret_key_base: "U1cptzGhnsZf/qeQeX9tyPxjqXlpM0ZeDkzoki285bwAfxupanYgIXv8vdx31XVg",
  render_errors: [view: UiWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Ui.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
