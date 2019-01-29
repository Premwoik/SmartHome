defmodule Core do
  use Application

  @moduledoc """
  Documentation for Core.
  """

  @mode System.get_env("MIX_MODE") || "normal"

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    opts = [strategy: :one_for_one, name: Core.Supervisor]
    Supervisor.start_link(children(@mode), opts)
  end

  def reload do
    :ok = Supervisor.stop(Device.Supervisor, :normal)
  end
  
  def children("no") do
    [Core.Actions]
  end

  def children(mode) do
    [
      Core.Device.Supervisor,
      Core.Actions,
      Core.Tasks
    ]
  end
end
