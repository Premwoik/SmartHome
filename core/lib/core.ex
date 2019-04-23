defmodule Core do
  use Application

  @moduledoc """
  Documentation for Core.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    #HTTPotion.start()
    opts = [strategy: :one_for_one, name: Core.Supervisor]
    Supervisor.start_link(children(), opts)
  end

  def reload do
    :ok = Supervisor.stop(Device.Supervisor, :normal)
  end

  def children() do
    [
      Core.Device.Supervisor,
      Core.Actions,
      Core.Tasks
    ]
  end
end
