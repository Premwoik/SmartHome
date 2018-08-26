
use Mix.Config

config :ui, UiWeb.Endpoint,
       url: [host: "192.168.2.100"],
       http: [port: 80],
       secret_key_base: "#############################",
       root: Path.dirname(__DIR__),
       server: true,
       render_errors: [view: UiWeb.ErrorView, accepts: ~w(html json)],
       pubsub: [name: Nerves.PubSub, adapter: Phoenix.PubSub.PG2],
       code_reloader: false


config :db, DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "/root/#{Mix.env}.sqlite3"

config :db, ecto_repos: [DB.Repo]