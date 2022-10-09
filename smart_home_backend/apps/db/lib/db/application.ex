defmodule DB.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias DB.MainRepo
  alias DB.InfluxConnection
  alias DB.Proc.ActionListProc
  alias DB.Proc.PortListProc
  alias DB.Proc.DeviceListProc
  alias DB.Proc.RfButtonListProc

  #  @otp_app Mix.Project.config()[:app]
  def start(_type, _args) do
    # List all child processes to be supervised

    children = [
      MainRepo,
      InfluxConnection,
      DeviceListProc,
      PortListProc,
      ActionListProc,
      RfButtonListProc
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: DB.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
