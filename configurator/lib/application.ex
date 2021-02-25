defmodule Configurator.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: Configurator.TaskSupervisor}
    ]

    opts = [strategy: :one_for_one, name: Configurator.Supervisor]
    Supervisor.start_link(children, opts)
  end

end
