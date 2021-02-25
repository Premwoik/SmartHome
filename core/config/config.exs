# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
# use Mix.Config
import Config

# config :logger,
#       backends: [:console],
#       compile_time_purge_level: :info

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

config :core, Core.Scheduler,
  timezone: "Europe/Warsaw",
  run_strategy: Quantum.RunStrategy.Local,
  jobs: [
    ia_h: [
      schedule: "@hourly",
      task: {DB.Stats.InputActivation, :collect, [:hourly]},
      timezone: "Europe/Warsaw"
    ],
    ia_d: [
      schedule: "@daily",
      task: {DB.Stats.InputActivation, :collect, [:daily]},
      timezone: "Europe/Warsaw"
    ],
    ia_w: [
      schedule: "@weekly",
      task: {DB.Stats.InputActivation, :collect, [:weekly]},
      timezone: "Europe/Warsaw"
    ],
    ia_m: [
      schedule: "@monthly",
      task: {DB.Stats.InputActivation, :collect, [:monthly]},
      timezone: "Europe/Warsaw"
    ],
    oa_h: [
      schedule: "@hourly",
      task: {DB.Stats.OutputActivation, :collect, [:hourly]},
      timezone: "Europe/Warsaw"
    ],
    oa_d: [
      schedule: "@daily",
      task: {DB.Stats.OutputActivation, :collect, [:daily]},
      timezone: "Europe/Warsaw"
    ],
    oa_w: [
      schedule: "@weekly",
      task: {DB.Stats.OutputActivation, :collect, [:weekly]},
      timezone: "Europe/Warsaw"
    ],
    oa_m: [
      schedule: "@monthly",
      task: {DB.Stats.OutputActivation, :collect, [:monthly]},
      timezone: "Europe/Warsaw"
    ],
    da_h: [
      schedule: "@hourly",
      task: {DB.Stats.DeviceActivation, :collect, [:hourly]},
      timezone: "Europe/Warsaw"
    ],
    da_d: [
      schedule: "@daily",
      task: {DB.Stats.DeviceActivation, :collect, [:daily]},
      timezone: "Europe/Warsaw"
    ],
    da_w: [
      schedule: "@weekly",
      task: {DB.Stats.DeviceActivation, :collect, [:weekly]},
      timezone: "Europe/Warsaw"
    ],
    da_m: [
      schedule: "@monthly",
      task: {DB.Stats.DeviceActivation, :collect, [:monthly]},
      timezone: "Europe/Warsaw"
    ],
    ta_h: [
      schedule: "@hourly",
      task: {DB.Stats.Temperature, :collect, [:hourly]},
      timezone: "Europe/Warsaw"
    ],
    ta_d: [
      schedule: "@daily",
      task: {DB.Stats.Temperature, :collect, [:daily]},
      timezone: "Europe/Warsaw"
    ],
    ta_w: [
      schedule: "@weekly",
      task: {DB.Stats.Temperature, :collect, [:weekly]},
      timezone: "Europe/Warsaw"
    ],
    ta_m: [
      schedule: "@monthly",
      task: {DB.Stats.Temperature, :collect, [:monthly]},
      timezone: "Europe/Warsaw"
    ],
    ea_h: [
      schedule: "@hourly",
      task: {DB.Stats.Energy, :collect, [:hourly]},
      timezone: "Europe/Warsaw"
    ],
    ea_d: [
      schedule: "@daily",
      task: {DB.Stats.Energy, :collect, [:daily]},
      timezone: "Europe/Warsaw"
    ],
    ea_w: [
      schedule: "@weekly",
      task: {DB.Stats.Energy, :collect, [:weekly]},
      timezone: "Europe/Warsaw"
    ],
    ea_m: [
      schedule: "@monthly",
      task: {DB.Stats.Energy, :collect, [:monthly]},
      timezone: "Europe/Warsaw"
    ]
  ]

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
import_config "#{Mix.env()}.exs"
