defmodule Core do
  use Application

  @moduledoc """
  Documentation for Core.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    opts = [strategy: :one_for_one, name: Core.Supervisor]
    :ok = create_quick_tasks_ets()
    :ok = Core.HandyConfig.init()
    Supervisor.start_link(children(), opts)
  end

  defp children() do
    [
      Core.Telemetry,
      Core.Scheduler,
      Core.Actions,
      Core.Mqtt.Supervisor,
      Core.Device.Supervisor
    ]
  end

  @ets_tasks_registry :core_tasks_registry

  defp create_quick_tasks_ets() do
    :ets.new(@ets_tasks_registry, [:named_table, :public])
    :ok
  end

  @spec run_task(atom()) :: :ok
  def run_task(name) do
    :ets.lookup(@ets_tasks_registry, name)
    |> Enum.each(fn {_, fun} -> fun.() end)

    :ok
  end

  @spec register_task(atom(), (() -> any())) :: :ok
  def register_task(name, fun) do
    :ets.insert_new(@ets_tasks_registry, {name, fun})
    :ok
  end

  @spec list_tasks() :: [{atom(), (() -> any())}]
  def list_tasks() do
    :ets.tab2list(@ets_tasks_registry)
  end
end
