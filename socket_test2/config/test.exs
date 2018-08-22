use Mix.Config


config :db,
       DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "itHome1.sqlite3"

config :db,
       ecto_repos: [DB.Repo]