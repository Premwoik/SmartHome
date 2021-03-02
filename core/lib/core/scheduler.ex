defmodule Core.Scheduler do
  @moduledoc false
  use Quantum, otp_app: :core
  #  import Crontab.CronExpression

  #  @action Core.Actions
  @actions Application.get_env(:core, :actions)
  require Logger
  alias DB.{ScheduleJob, Repo}

  def initialize() do
    ScheduleJob.all()
    |> Enum.each(&add_action_job/1)
  end

  def reload(jobs) when is_list(jobs), do: Enum.map(jobs, &reload/1)

  def reload(%{__struct__: ScheduleJob, id: id} = job) do
    name = String.to_atom("db_#{id}")
    Core.Scheduler.delete_job(name)
    add_action_job(job)
  end

  def add_action_job(%{id: id, expr: expr, extended: e} = job) do
    name = String.to_atom("db_#{id}")
    add_new_job(name, expr, e, handle_job(job))

    if(!job.active) do
      Core.Scheduler.deactivate_job(name)
    else
      :ok
    end
  end

  @spec add_new_job(atom, String.t(), boolean, (() -> none)) :: any
  def add_new_job(name, cron_string, extended, task) do
    with {:ok, schedule} <- Crontab.CronExpression.Parser.parse(cron_string, extended) do
      __MODULE__.new_job()
      |> Quantum.Job.set_name(name)
      |> Quantum.Job.set_schedule(schedule)
      |> Quantum.Job.set_task(task)
      |> Quantum.Job.set_timezone("Europe/Warsaw")
      |> __MODULE__.add_job()
    else
      _ -> Logger.error("Cron expr is wrong in job #{name}")
    end
  end

  defp handle_job(%{action_id: id, state: :up}),
    do: fn -> @actions.activate_up([Repo.id(id)]) end

  defp handle_job(%{action_id: id, state: :down}),
    do: fn -> @actions.activate_down([Repo.id(id)]) end
end
