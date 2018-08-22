defmodule SocketTest2 do
  use Application
  @moduledoc """
  Documentation for SocketTest2.
  """
  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      Device.Supervisor
    ]
    opts = [strategy: :one_for_one, name: Main.Supervisor]

    Supervisor.start_link(children, opts)
  end

  def reload do
    :ok = Supervisor.stop(Device.Supervisor, :normal)
  end

end
