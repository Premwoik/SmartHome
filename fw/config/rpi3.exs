
use Mix.Config


config :db, DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "/root/#{Mix.env}.sqlite3"

config :db, ecto_repos: [DB.Repo]