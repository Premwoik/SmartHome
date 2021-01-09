defmodule Core.Tasks do
  @moduledoc """

    ###Server


    (call callbacks are not necessary now)
    //call:
    //  - start_task
    //  - restart_task
    //  - stop_task
    cast:
      - refresh_tasks +
      - waiting +
      - finish_task +

  """

  use GenServer
  use Timex
  require Logger

  alias Core.Utils.DateTime, as: DT

  #  alias DB.{Task}

  # Client

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @doc """

  """
  def update(name \\ __MODULE__) do
    GenServer.cast(name, :update_tasks)
  end

  # CALLBACKS

  @impl true
  @doc """
    Init data and load tasks from database.
    This function is invoke only during initialization, so all running task
    should be threaten as killed - should be add to waiting
  """
  def init(_args) do
    Logger.debug("Initlializing tasks process with pid #{inspect(self())}")
    waiting = DB.Task.get_active()
    send(self(), :waiting)
    {:ok, %{started: [], waiting: waiting, timer_ref: nil}}
  end

  #  @impl true
  #  @doc """
  #
  #  """
  #  def handle_cast(:update_task, %{timer_ref: ref, started: starded} = s) do
  #  end

  @impl true
  @doc """
    Refresh own task's datas to values that are in the database. If task status changed, task process should follow that change, (It means starting new task or ending actually runnig if needed)
  """
  def handle_cast(:update_tasks, %{timer_ref: ref, started: started} = s) do
    if ref, do: Process.cancel_timer(ref)

    {running, waiting} =
      DB.Task.get_active()
      |> Enum.split_with(fn %{status: status} -> status == "running" end)

    if length(waiting) > 0, do: send(self(), :waiting)

    {:noreply, %{s | timer_ref: ref, started: update_started(started, running), waiting: waiting}}
  end

  @impl true
  @doc """
    Mark task as finished and remove it from running task list. This function must be called after task process finished.
  """
  def handle_cast({:finish_task, task}, %{started: started} = s) do
    Logger.debug("Finishing task - #{inspect(task.name)}")
    still_started = Enum.filter(started, fn {_, t} -> t != task end)
    DB.Task.update(task, status: "inactive")
    {:noreply, %{s | started: still_started}}
  end

  @impl true
  @doc """
    Start process for single task when it is time for it  
  """
  def handle_info(:waiting, %{started: started, waiting: waiting} = s) do
    Logger.debug("Waiting tasks #{inspect(Enum.map(waiting, fn x -> x.name end))}")
    ready = Enum.filter(waiting, fn x -> x.start_date == nil || high_time?(x.start_date) end)
    just_started = Enum.map(ready, fn task -> {task, initialize_executor(task)} end)
    still_waiting = Enum.sort(waiting -- ready)
    all_started = started ++ just_started
    ref = set_timer(still_waiting)
    {:noreply, %{s | timer_ref: ref, waiting: still_waiting, started: all_started}}
  end

  # Private

  defp update_started(actual, new, accu \\ [])
  defp update_started([], _new, accu), do: accu

  defp update_started([{task, {:ok, pid}} | tail], new, accu) do
    case Enum.find(new, fn %{id: id} -> id == task.id end) do
      nil ->
        send(pid, :force_stop)
        update_started(tail, new, accu)

      new_task ->
        send(pid, {:update_task, new_task})
        update_started(tail, new, [{new_task, {:ok, pid}} | accu])
    end
  end

  defp set_timer([]), do: nil

  defp set_timer(still_waiting) do
    wait_time =
      List.first(still_waiting).start_date
      |> DT.to_unix_from_now(:millisecond)

    Process.send_after(self(), :waiting, wait_time)
  end

  defp high_time?(start_date) do
    DT.now_after?(start_date)
  end

  defp initialize_executor(task) do
    Logger.debug("Running task #{inspect(task.name)}")
    DB.Task.update(task, status: "running")
    Task.start_link(fn -> execution_loop(task, module(task).init_state()) end)
  end

  # SINGLE TASK CONTROLLING PROCESS

  defp execution_loop(task, state, num \\ 0) do
    receive do
      {:update_task, task_} ->
        execution_loop(task_, state, num)

      :force_stop ->
        GenServer.cast(__MODULE__, {:finish_task, task})
        # {:ok, {task, state, num}}
    after
      get_wait_time(task) ->
        new_state =
          module(task).execute(task, state)
          |> case do
            {:ok, state_} -> state_
            :ok -> state
            :error -> state
          end

        if next_execution?(task, num) do
          execution_loop(task, new_state, num - 1)
        else
          GenServer.cast(__MODULE__, {:finish_task, task})
        end
    end
  end

  defp get_wait_time(%{execution_time: nil, frequency: freq}), do: freq

  defp get_wait_time(%{execution_time: time}) do
    Time.diff(time, Time.utc_now(), :milliseconds)
    |> (fn
          t when t < 0 -> 86_399_000 + t
          t -> t
        end).()
  end

  defp next_execution?(%{end_date: end_date, limit: limit}, num)
       when limit > num or limit == -1,
       do: before_end_date?(end_date)

  #  defp next_execution?(_), do: false

  defp before_end_date?(nil), do: true
  defp before_end_date?(end_date), do: DT.now_before?(end_date)

  defp module(task) do
    String.to_existing_atom("Elixir." <> task.type.module)
  end
end
