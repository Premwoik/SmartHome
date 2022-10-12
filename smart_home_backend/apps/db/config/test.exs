use Mix.Config

config :db, DB.MainRepo,
  username: "postgres",
  password: "postgres",
  database: "smart_home_test",
  hostname: "192.168.2.100",
  port: 5433,
  pool: Ecto.Adapters.SQL.Sandbox

config :db, DB.StatsRepo,
  username: "postgres",
  password: "postgres",
  database: "smart_home_statistics_test",
  hostname: "192.168.2.100",
  port: 5433,
  pool: Ecto.Adapters.SQL.Sandbox
