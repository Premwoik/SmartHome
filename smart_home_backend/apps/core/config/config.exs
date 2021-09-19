# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
# use Mix.Config
import Config

# config :logger,
# backends: [:console],
# compile_time_purge_level: :warn

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :socket_test2, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:socket_test2, :key)
#
# You can also configure a 3rd-party app:
#
# config :logger, level: :info
##

# config :core, :devices_module, Core.Devices

config :core, two_way_client: Core.Device.Client.TwoWay
config :core, one_way_client: Core.Device.Client.OneWay

config :core, actions: Core.Actions
config :core, device_helper: Core.Device

config :core, database_module: DB

# config :socket_test2,
if Mix.target() == :core do
  import_config "../../db/config/config.exs"
end

if Mix.target() == :coret do
  import_config "../../db/config/config.exs"

  config :core, actions: Core.Utils.Mock.Actions
end

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
import_config "#{Mix.env()}.exs"
