import Config

config :db,
       DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "../db/itHome.sqlite3",
       log: false