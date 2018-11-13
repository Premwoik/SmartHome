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
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
  end

  #   Other scopes may use custom stacks.
  scope "/api", UiWeb do
    pipe_through :api

    get "/pages/short", PageController, :short
    get "/pages/content/:id", PageController, :page_content
    get "/pages/view/:id", PageController, :page

    post "/dimmers/toggle", DimmerController, :toggle
    post "/dimmers/setOn", DimmerController, :set_on
    post "/dimmers/setOff", DimmerController, :set_off
    post "/dimmers/setBrightness", DimmerController, :set_brightness
    post "/dimmers/setLightOff", DimmerController, :set_light_off
    post "/dimmers/setLightOn", DimmerController, :set_light_on

    post "/lights/set_brightness", LightController, :set_brightness
    post "/lights/toggle", LightController, :toggle
    post "/lights/setOn", LightController, :set_on
    post "/lights/setOff", LightController, :set_off

    post "/sunblinds/click", SunblindController, :click
    post "/sunblinds/calibrate", SunblindController, :calibrate

    post "/actions/toggle", ActionController, :toggle
    post "/actions/setOn", ActionController, :set_on
    post "/actions/setOff", ActionController, :set_off

    resources "/lights", LightController
    resources "/dimmers", DimmerController
    resources "/sunblinds", SunblindController
    resources "/scenes", SceneController
    resources "/actions", ActionController
  end
end
