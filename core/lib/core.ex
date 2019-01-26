defmodule Core do
  use Application
  @moduledoc """
  Documentation for Core.
  """
  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      Core.Device.Supervisor,
      Core.Actions,
      Core.Tasks
    ]
    opts = [strategy: :one_for_one, name: Core.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def reload do
    :ok = Supervisor.stop(Device.Supervisor, :normal)
  end


end
