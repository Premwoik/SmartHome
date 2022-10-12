defmodule DB.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  #  @otp_app Mix.Project.config()[:app]
  def start(_type, _args) do
    # List all child processes to be supervised
    #    setup_db!(type)

    children = [
      DB.StatsRepo
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DB.Supervisor]
    Supervisor.start_link(children, opts)
    #    if type != :test, do: DB.Init.run()
    #    res
  end

end
