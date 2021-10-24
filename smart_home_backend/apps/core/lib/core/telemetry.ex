defmodule Core.Telemetry do
  use Supervisor
  require Logger
  # import Telemetry.Metrics
  alias Core.Telemetry.DeviceMetrics
  alias Core.Telemetry.ActionMetrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def init(_arg) do
    :ok =
      :telemetry.attach(
        "report device execute",
        [:core, :device, :do],
        &DeviceMetrics.make_report/4,
        nil
      )

    :ok =
      :telemetry.attach(
        "report action execute",
        [:core, :action, :run],
        &ActionMetrics.make_report/4,
        nil
      )

    Supervisor.init([], strategy: :one_for_one)
  end

  def metrics do
    []
  end
end
