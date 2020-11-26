import Config

config :db,
       DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "/media/pi/677c332c-d42b-42a5-bddc-f53931aa9c55/itHome.sqlite3",
       log: false