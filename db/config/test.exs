import Config

config :db,
       DB.Repo,
       adapter: Sqlite.Ecto2,
       database: "itHome_test1.sqlite3",
       size: 1,
       max_overflow: 0,
       loggers: [{Ecto.LogEntry, :log, [:info]}]