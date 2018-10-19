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

  alias Core.Utils.DateTime, as: DT


  #  alias DB.{Task}

  # Client

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @doc """

  """
  def reload(name \\ __MODULE__) do
    GenServer.call(name, :reload_tasks)
  end


  def finish_task(task, name \\ __MODULE__) do
    Genserver.call(name, {:finish_task, task})
  end


  # Server

  @doc """
    Init data and load tasks from database.
    This function is invoke only during initialization, so all running task
    should be threaten as killed - should be add to waiting
  """

  @impl true
  def init(args) do
    waiting = DB.Task.get_active()
    send(self(), :waiting)
    {:ok, %{started: [], waiting: waiting, timer_ref: nil}}
  end


  @impl true
  def handle_call(:refresh_tasks, %{started: started, waiting: waiting} = s) do
    DB.Task.get_active()
    |> Enum.filter(fn x -> x.status == "waiting" end)
    |> fn new -> {:noreply, %{s | waiting: waiting ++ new}} end.()
  end


  @impl true
  def handle_call({:finish_task, task}, %{started: started} = s) do
    still_started = Enum.filter started, fn {_, t} -> t != task  end
    :ok = DB.Task.update(task, status: "end")
    {:noreply, %{s | started: still_started}}
  end


  @impl true
  def handle_info(:waiting, %{started: started, waiting: waiting} = s) do
    ready = Enum.filter waiting, fn x -> x.start_date == nil || high_time?(x.start_date) end
    just_started = Enum.map ready, fn task -> {task, initialize_executor task} end
    still_waiting = Enum.sort(waiting -- ready)
    all_started = started ++ just_started
    ref = set_timer still_waiting
    {:noreply, %{s | timer_ref: ref, waiting: still_waiting, started: all_started}}
  end

  # Private

  defp set_timer([]), do: nil
  defp set_timer(still_waiting) do
    wait_time = (List.first still_waiting).start_date
                |> DT.to_unix_from_now(:millisecond)
    Process.send_after(self(), :waiting, wait_time)
  end

  defp high_time?(start_date) do
    DT.now_after?(start_date)
  end

  defp initialize_executor(task) do
    DB.Task.update(task, status: "running")
    Task.start_link fn -> execution_loop task, module(task).init_state() end
  end

  defp execution_loop(task, state, num \\ 0) do
    receive do
    after
      task.frequency ->
        new_state =
          module(task).execute(task.action, task.device, state)
          |> case do
               {:ok, state_} -> state_
               :error -> state
             end
        if next_execution?(task, num) do
          execution_loop(task, new_state, num - 1)
        else
          GenServer.cast(__MODULE__, {:finish_task, task})
        end
    end
  end


  defp next_execution?(%{end_date: end_date, limit: limit}, num)
       when limit > num or limit == -1, do: before_end_date? end_date
  defp next_execution?(_), do: false

  defp before_end_date?(nil), do: true
  defp before_end_date?(end_date), do: DT.now_before?(end_date)

  defp module(task) do
    String.to_existing_atom "Elixir." <> task.type.module
  end

end
