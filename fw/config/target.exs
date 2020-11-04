
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

config :nerves_firmware_ssh,
       authorized_keys: [
         File.read!(Path.join(System.user_home!, ".ssh/id_rsa.pub"))
       ]

config :nerves_init_gadget,
       ifname: "eth0",
       address_method: :dhcp,
       mdns_domain: "home.local",
       node_name: "rpi3",
       ssh_console_port: 22

config :tzdata, :autoupdate, :disabled

import_config "../../core/config/config.exs"
