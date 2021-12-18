import Config

config :db, DB.StatsRepo,
  adapter: Ecto.Adapters.Postgres,
  database: "smart_home_statistics",
  username: "postgres",
  password: "postgres",
  hostname: "192.168.2.100",
  port: 5433

config :db, DB.MainRepo,
  adapter: Ecto.Adapters.Postgres,
  database: "smart_home",
  username: "postgres",
  password: "postgres",
  hostname: "192.168.2.100",
  port: 5433
