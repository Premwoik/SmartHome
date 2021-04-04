defmodule DB.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  #  @otp_app Mix.Project.config()[:app]
#  @env Application.get_env(:db, :env)
  def start(_type, _args) do
    # List all child processes to be supervised
    children = case Mix.env() do
      :test -> []
      _else -> [DB.StatsRepo]
    end
    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DB.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
