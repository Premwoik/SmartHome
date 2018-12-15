use Mix.Config

import_config "../../core/config/config.exs"
import_config "../../db/config/config.exs"
import_config "../../ui/config/config.exs"

#config :ui, UiWeb.Endpoint,
#       url: [host: "192.168.2.119"],
#       http: [port: 4000],
#       secret_key_base: "#############################",
#       root: Path.dirname(__DIR__),
#       server: true,
#       render_errors: [view: UiWeb.ErrorView, accepts: ~w(html json)],
#       pubsub: [name: Nerves.PubSub, adapter: Phoenix.PubSub.PG2],
#       code_reloader: false
