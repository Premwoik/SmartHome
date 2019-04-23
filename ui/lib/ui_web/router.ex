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

    get "/devices/types/all", DeviceController, :get_types

    get "/dashboards/view/short", DashboardController, :short
    get "/dashboards/view/:id", DashboardController, :view

    post "/dimmers/setOn/:id", DimmerController, :set_on
    post "/dimmers/setOff/:id", DimmerController, :set_off
    post "/dimmers/setLightOff/:id", DimmerController, :set_light_off
    post "/dimmers/setLightOn/:id", DimmerController, :set_light_on
    post "/dimmers/setBrightness", DimmerController, :set_brightness


    post "/lights/setOn/:id", LightController, :set_on
    post "/lights/setOff/:id", LightController, :set_off

    post "/sunblinds/click/:id", SunblindController, :click
    post "/sunblinds/calibrate", SunblindController, :calibrate

    post "/actions/setOn/:id", ActionController, :set_on
    post "/actions/setOff/:id", ActionController, :set_off
    post "/actions/update_args/:id", ActionController, :update_args
    get "/actions/get_args/:id", ActionController, :get_args

    post "/ports/setOn/:id", PortController, :set_on
    post "/ports/setOff/:id", PortController, :set_off

    post "/tasks/setOn/:id", TaskController, :set_on
    post "/tasks/setOff/:id", TaskController, :set_off
    get "/tasks/types/all", TaskController, :get_types

    get "/stats/device-journal/report/:id", DeviceJournalController, :get_report
    get "/stats/device-journal/report_sec/:id", DeviceJournalController, :get_report_s

    get "/stats/device-journal/logs/:id", DeviceJournalController, :get_logs

  end


end
