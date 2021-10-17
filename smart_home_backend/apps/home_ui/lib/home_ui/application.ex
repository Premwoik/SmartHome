defmodule HomeUi.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    # Setup core handy config
    setup_core_handy_config()

    children = [
      # Start the Ecto repository
      # HomeUi.Repo,
      # Start the Telemetry supervisor
      HomeUiWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: HomeUi.PubSub},
      # Start the Endpoint (http/https)
      HomeUiWeb.Endpoint
      # Start a worker by calling: HomeUi.Worker.start_link(arg)
      # {HomeUi.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: HomeUi.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    HomeUiWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  def setup_core_handy_config() do
    Core.HandyConfig.register_broadcast_handler(HomeUiWeb.Channels.BroadcastHandler)
  end
end
