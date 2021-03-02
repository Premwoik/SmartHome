import Config

# config :db,
#       DB.Repo,
#       adapter: Sqlite.Ecto2,
#       database: "../db/itHome.sqlite3",
#       log: false

# config :db,
#       DB.StatsRepo,
#       adapter: Sqlite.Ecto2,
#       database: "../db/statsDB.sqlite3"

config :db, DB.StatsRepo,
  adapter: Ecto.Adapters.Postgres,
  database: "smart_home_statistics",
  username: "sh_admin",
  password: "mVu93~`G",
  hostname: "192.168.2.100",
  port: 5433

# music VISA usa 9 3 ~ ` GOLF
#       log: false
