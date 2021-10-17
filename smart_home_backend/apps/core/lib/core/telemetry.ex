defmodule Core.Telemetry do
  use Supervisor
  require Logger
  import Telemetry.Metrics

  alias DB.Stats.DeviceJournal, as: DJ

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def init(_arg) do
    :ok = :telemetry.attach("report device execute", [:core, :device, :do], &make_report/4, nil)
    # :ok = :telemetry.attach("report action run", [:core, :action, :run], &make_report2/4, nil)

    # :ok =
    # :telemetry.attach(
    # "monitor job execution",
    # [:quantum, :job, :start],
    # &monitor_job_execution/4,
    # nil
    # )

    children = []
    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    []
  end

  def monitor_job_execution([:quantum, :job, :start], _mesurements, _meta, nil) do
    # TODO
  end

  def make_report2(
        [:core, :action, :run],
        %{duration: duration},
        %{action: action, mode: mode},
        nil
      ) do
    Logger.info(
      "Invoke action [id=#{action.id}, name=#{action.name}] in [mode=#{inspect(mode)}, time=#{duration}s]"
    )
  end

  def make_report(
        [:core, :device, :do],
        %{duration: duration, result: res, args: args},
        %{function: fun, device: device},
        nil
      ) do
    case fun do
      :set_outputs ->
        name = "missing"

        case args do
          [ports] ->
            ids = Enum.map(ports, fn %{name: name, state: %{"value" => val}} -> {name, val} end)
            Logger.info("Set outputs - [device=#{device.name}] [ports=#{inspect(ids)}]")

          _ ->
            :ok
        end

      _ ->
        :ok
    end

    case res do
      %{error: []} ->
        :ok

      _ ->
        Logger.error(
          "Execute device call [id=#{device.id}] [cmd=#{fun}] with [status=#{inspect(:error)}] in [time=#{duration}s]"
        )
    end

    DJ.log_use(device.id, res, fun)
  end

  defp pretty_args([]) do
    ""
  end

  defp pretty_args([%DB.Data.Port{} = port | tail]) do
    state = port.state["value"]
    "   #{inspect(%{id: port.id, name: port.name, state: state})} \n" <> pretty_args(tail)
  end

  defp pretty_args([sth | tail]) do
    "   #{inspect(sth)} \n" <> pretty_args(tail)
  end
end
