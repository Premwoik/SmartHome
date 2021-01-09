defmodule UiWeb.Router do
  use UiWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", UiWeb do
    pipe_through :browser
    get "/", PageController, :index
  end

  # Other scopes may use custom stacks.
  scope "/api", UiWeb do
    pipe_through :api

    resources "/dimmers", DimmerController, except: [:new, :edit]
    resources "/lights", LightController, except: [:new, :edit]
    resources "/ports", PortController, except: [:new, :edit]
    resources "/actions", ActionController, except: [:new, :edit]
    resources "/devices", DeviceController, except: [:new, :edit]
    resources "/dashboards", DashboardController, except: [:new, :edit]
    resources "/sunblinds", SunblindController, except: [:new, :edit]
    resources "/tasks", TaskController, except: [:new, :edit]
    resources "/thermometers", ThermometerController, except: [:new, :edit]
    resources "/energy_meters", EnergyMeterController, except: [:new, :edit]
    resources "/rf_buttons", RfButtonController, except: [:new, :edit]
    resources "/alarm_partitions", AlarmPartitionController, except: [:new, :edit]

    get "/devices/types/all", DeviceController, :get_types

    get "/dashboards/view/short", DashboardController, :short
    get "/dashboards/view/:id", DashboardController, :view

    post "/dimmers/setOn/:id", DimmerController, :set_on
    post "/dimmers/setOff/:id", DimmerController, :set_off
    post "/dimmers/setBrightness", DimmerController, :set

    post "/lights/setOn/:id", LightController, :set_on
    post "/lights/setOff/:id", LightController, :set_off
    post "/lights/set/:id/:state", LightController, :set

    post "/sunblinds/click/:id", SunblindController, :click
    post "/sunblinds/calibrate", SunblindController, :calibrate

    post "/actions/setOn/:id", ActionController, :set_on
    post "/actions/setOff/:id", ActionController, :set_off
    post "/actions/activate_high/:id", ActionController, :activate_high
    post "/actions/activate_low/:id", ActionController, :activate_low
    post "/actions/set/:id/:state", ActionController, :set
    post "/actions/update_args/:id", ActionController, :update_args
    get "/actions/get_args/:id", ActionController, :get_args
    get "/actions/get_items/:id", ActionController, :get_items

    post "/ports/setOn/:id", PortController, :set_on
    post "/ports/setOff/:id", PortController, :set_off

    post "/tasks/setOn/:id", TaskController, :set_on
    post "/tasks/setOff/:id", TaskController, :set_off
    get "/tasks/types/all", TaskController, :get_types

    get "/stats/device-journal/report/:id", DeviceJournalController, :get_report
    get "/stats/device-journal/report_sec/:id", DeviceJournalController, :get_report_s
    get "/stats/device-journal/logs/:id", DeviceJournalController, :get_logs
    get "/stats/activations/devices/:id", ActivationsHistoryController, :get_device_activations
    get "/stats/activations/inputs/:id", ActivationsHistoryController, :get_input_activations
    get "/stats/activations/outputs/:id", ActivationsHistoryController, :get_output_activations

    get "/thermometers/get_temperature/:id", MeterReadingsController, :get_temperature
    get "/energy_meters/get_energy/:id", MeterReadingsController, :get_energy
    #    get "/thermometers/get_temperature/:id", ThermometerController, :get_temperature

    post "/alarm_partitions/arm/:id", AlarmPartitionController, :arm
    post "/alarm_partitions/disarm/:id", AlarmPartitionController, :disarm
    post "/alarm_partitions/clear_alarm/:id", AlarmPartitionController, :clear_alarm

    get "/schema/port", SchemaController, :get_port_schema
    get "/schema/light", SchemaController, :get_light_schema
    get "/schema/dimmer", SchemaController, :get_dimmer_schema
    get "/schema/sunblind", SchemaController, :get_sunblind_schema
    get "/schema/task", SchemaController, :get_task_schema
    get "/schema/action", SchemaController, :get_action_schema
    get "/schema/rf_button", SchemaController, :get_rf_button_schema
    get "/schema/device", SchemaController, :get_device_schema
  end
end
