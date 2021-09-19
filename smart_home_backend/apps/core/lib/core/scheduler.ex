defmodule Core.Scheduler do
  @moduledoc false
  use Quantum, otp_app: :core

  require Logger
  alias Core.Actions
  alias DB.Data.{ScheduleJob}
  alias Quantum.Job

  @doc "Adds a job from a ScheduleJob structure."
  @spec add_schedule_job(ScheduleJob.t()) :: %Job{} | nil
  def add_schedule_job(%ScheduleJob{} = job) do
    with {:ok, schedule} <- Crontab.CronExpression.Parser.parse(job.expr, job.extended) do
      new_job()
      |> Job.set_name(String.to_atom(job.name))
      |> Job.set_schedule(schedule)
      |> Job.set_task(get_task(job.task))
    else
      _ ->
        Logger.error("Cron expr is wrong for a job - id: #{job.id}")
        nil
    end
  end

  def get_task(%{"action_id" => id} = task) do
    case Map.get(task, "state", "up") do
      "up" -> fn -> Actions.activate_up([id]) end
      "down" -> fn -> Actions.activate_down([id]) end
      _ -> fn -> Logger.warn("Wrong task state. Allowed [up|down].") end
    end
  end

  @impl true
  def init(config) do
    jobs =
      ScheduleJob.list_all!()
      |> Enum.map(&add_schedule_job/1)
      |> Enum.filter(&(!is_nil(&1)))

    Keyword.put(config, :jobs, jobs)
  end
end
