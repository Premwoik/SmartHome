defmodule Core.Scheduler do
  @moduledoc false
  use Quantum, otp_app: :core

  require Logger
  alias Core.Actions
  alias DB.Data.{ScheduleJob}
  alias Quantum.Job

  @doc "Reload one job from DB"
  @spec reload_job_from_db(integer()) :: :ok
  def reload_job_from_db(id) do
    with %ScheduleJob{} = job <- ScheduleJob.get!(id),
         %Job{} = qjob <- add_schedule_job(job) do
      :ok = delete_job(qjob.name)
      :ok = add_job(qjob)
    end

    :ok
  end

  @doc "Delete all jobs and reinitialize them from database"
  @spec reload_jobs_from_db() :: :ok
  def reload_jobs_from_db() do
    :ok = delete_all_jobs()

    load_and_prepare_jobs()
    |> Enum.each(&add_job/1)

    :ok
  end

  @doc "Adds a job from a ScheduleJob structure."
  @spec add_schedule_job(ScheduleJob.t()) :: %Job{} | nil
  def add_schedule_job(%ScheduleJob{} = job) do
    with {:ok, schedule} <- Crontab.CronExpression.Parser.parse(job.expr, job.extended) do
      new_job()
      |> Job.set_name(String.to_atom(job.name))
      |> Job.set_schedule(schedule)
      |> Job.set_task(get_task(job.task))
      |> Job.set_state(get_state(job))
    else
      _ ->
        Logger.error("Cron expr is wrong for a job - id: #{job.id}")
        nil
    end
  end

  defp get_state(%{status: status}) do
    if status, do: :active, else: :inactive
  end

  @doc "Create func that runs action wiht up or down signal."
  def get_task(%{"action_id" => id} = task) do
    case Map.get(task, "state", "up") do
      "up" -> fn -> Actions.activate_up([id]) end
      "down" -> fn -> Actions.activate_down([id]) end
      _ -> fn -> Logger.warn("Wrong task state. Allowed [up|down].") end
    end
  end

  @impl true
  def init(config) do
    jobs = load_and_prepare_jobs()
    register_quick_tasks(jobs)
    Keyword.put(config, :jobs, jobs)
  end

  defp load_and_prepare_jobs() do
    ScheduleJob.list_all!()
    |> Enum.map(&add_schedule_job/1)
    |> Enum.filter(&(!is_nil(&1)))
  end

  defp register_quick_tasks(jobs) do
    Enum.map(jobs, &Core.register_task(&1.name, &1.task))
  end

  defmodule Ctl do
    alias Core.Scheduler

    def find_job(name), do: Scheduler.find_job(name)

    def jobs(), do: Scheduler.jobs()

    def add_job(), do: Scheduler.add_job()

    def delete_job(), do: Scheduler.delete_job()

    def run_job(name), do: Scheduler.run_job(name)

    def activate_job(name), do: Scheduler.activate_job(name)

    def deactivate_job(name), do: Scheduler.deactivate_job(name)
  end
end
