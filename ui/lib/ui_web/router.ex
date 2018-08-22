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
    plug CORSPlug, origin: "http://192.168.2.119:4000"
    plug :accepts, ["json"]
  end

  scope "/", UiWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
  end

#   Other scopes may use custom stacks.
   scope "/api", UiWeb do
     pipe_through :api

     post "/dimmers/set_fill", DimmerController, :set_fill
     post "/dimmers/toggle", DimmerController, :toggle
     post "/dimmers/toggle_light", DimmerController, :toggle_light

     post "/sunblinds/toggle", SunblindController, :toggle
     post "/sunblinds/toggle_one", SunblindController, :toggle_one
     post "/sunblinds/set_position", SunblindController, :set_position



     resources "/lights", LightController
     resources "/dimmers", DimmerController
     resources "/sunblinds", SunblindController
     resources "/scenes", SceneController
   end
end
