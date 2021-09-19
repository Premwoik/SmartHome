defmodule Core.Telemetry do
  use Supervisor
  import Telemetry.Metrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def init(_arg) do
    children = []

    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    [

    ]
  end

end
