import Config

config :db, DB.StatsRepo,
       adapter: Ecto.Adapters.Postgres,
       database: "smart_home_statistics",
       username: "sh_admin",
       password: "mVu93~`G",
       hostname: "192.168.2.100",
       port: 5433
