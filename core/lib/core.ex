defmodule Core do
  use Application

  @moduledoc """
  Documentation for Core.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    opts = [strategy: :one_for_one, name: Core.Supervisor]
    sup_res = Supervisor.start_link(children(), opts)
    initialize()
    sup_res
  end

  def initialize() do
    Core.Scheduler.initialize()
  end

  def reload do
    :ok = Supervisor.stop(Device.Supervisor, :normal)
  end

  def children() do
    [
      Core.Scheduler,
      Core.Actions,
      Core.Mqtt.Supervisor,
      Core.Device.Supervisor
    ]
  end
  @doc"""
    propagate_item_update is a function that can be used to notify all processes,
      which uses copy of object, that its db version was updated

  """
  def propagate_item_update(%DB.ScheduleJob{} = job) do
    :ok = Core.Scheduler.reload(job)
    job
  end
  def propagate_item_update(job), do: job
end
