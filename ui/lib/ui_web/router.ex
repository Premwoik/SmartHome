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

    get "/dashboards/view/short", DashboardController, :short
    get "/dashboards/view/:id", DashboardController, :view


    get "/dimmers/cardView/:id", DimmerController, :dash_show
    post "/dimmers/setOn/:id", DimmerController, :set_on
    post "/dimmers/setOff/:id", DimmerController, :set_off
    post "/dimmers/setLightOff/:id", DimmerController, :set_light_off
    post "/dimmers/setLightOn/:id", DimmerController, :set_light_on
    post "/dimmers/setBrightness", DimmerController, :set_brightness


    get "/lights/cardView/:id", LightController, :dash_show
    post "/lights/setOn/:id", LightController, :set_on
    post "/lights/setOff/:id", LightController, :set_off


    get "/sunblinds/cardView/:id", SunblindController, :dash_show
    post "/sunblinds/click/:id", SunblindController, :click
    post "/sunblinds/calibrate", SunblindController, :calibrate

    get "/actions/cardView/:id", ActionController, :dash_show
    post "/actions/setOn/:id", ActionController, :set_on
    post "/actions/setOff/:id", ActionController, :set_off


    get "/ports/cardView/:id", PortController, :dash_show
    post "/ports/setOn/:id", PortController, :set_on
    post "/ports/setOff/:id", PortController, :set_off


    get "/tasks/cardView/:id", TaskController, :dash_show
    post "/tasks/setOn/:id", TaskController, :set_on
    post "/tasks/setOff/:id", TaskController, :set_off

  end


end
