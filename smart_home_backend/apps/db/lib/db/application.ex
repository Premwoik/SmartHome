defmodule DB.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias DB.Proc.ActionListProc
  alias DB.Proc.PortListProc

  #  @otp_app Mix.Project.config()[:app]
  def start(_type, _args) do
    # List all child processes to be supervised

    children = [
      DB.MainRepo,
      DB.StatsRepo,
      PortListProc,
      ActionListProc
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DB.Supervisor]
    Supervisor.start_link(children, opts)
  end
end