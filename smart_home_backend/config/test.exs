import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.

# if System.get_env("CI") do
# config :home_ui, HomeUi.Repo,
# url:
# "postgres://postgres:postgres@postgres:5432/utility_test#{
# System.get_env("MIX_TEST_PARTITION")
# }",
# pool: Ecto.Adapters.SQL.Sandbox
# else
# config :home_ui, HomeUi.Repo,
# database: "home_ui_test#{System.get_env("MIX_TEST_PARTITION")}",
# hostname: "localhost",
# pool: Ecto.Adapters.SQL.Sandbox
# end

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :ui, UiWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "w9WasS/mTPjiFYgsX7YzO8G5GTMfhb9FSRMDxrLlo6ftzPJfkYlUJsboKMzWSG5a",
  server: false

# In test we don't send emails.
config :ui, Ui.Mailer, adapter: Swoosh.Adapters.Test

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Print only warnings and errors during test
config :logger, level: :warn

config :db, DB.MainRepo,
  username: "postgres",
  password: "postgres",
  database: "smart_home_test",
  hostname: "192.168.2.118",
  port: 5433,
  pool: Ecto.Adapters.SQL.Sandbox
