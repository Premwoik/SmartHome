defmodule Core.TasksTest do
  @moduledoc false
  use ExUnit.Case

  alias Core.Controllers.BasicController
  import Mox

  setup :verify_on_exit!

  setup :set_mox_global

  test "task with execution limit " do

    pid = self()

    Core.Tasks.TaskMock
    |> expect(:execute, 3, fn id, device, state -> send(pid, state.id); {:ok, %{state | id: state.id + 1}} end)
    |> expect(:init_state, fn -> %{id: 0} end)

    DB.Task.update_status([1, 2], "inactive")
    DB.Task.update_status([1], "waiting")

    {:ok, pid} = Core.Tasks.start_link(nil)

    assert receive_msgs(3) == [2, 1, 0]

    Process.exit(pid, :normal)

  end

  test "task with period " do

    pid = self()

    d_s = ~N[2018-10-18 18:00:00.000000]
    d_e = ~N[2018-10-18 19:00:00.000000]
    d_n = ~N[2018-10-18 18:01:00.000000]

    Core.Tasks.TaskMock
    |> expect(:execute, 3, fn id, device, state -> send(pid, state.id); {:ok, %{state | id: state.id + 1}} end)
    |> expect(:init_state, fn -> %{id: 0} end)

    Core.Utils.DateTime.Mock
    |> expect(:now, 3, fn -> ~N[2018-10-18 18:01:00.000000] end)
    |> expect(
         :compare,
         3,
         fn t1, t2 ->
           cond do
             t1 == d_n && t2 == d_e -> :lt
             t1 == d_n && t2 == d_s -> :gt
             true -> :error
           end
         end
       )

    DB.Task.update_status([1, 2], "inactive")
    DB.Task.update_status([2], "waiting")

    {:ok, pid} = Core.Tasks.start_link(nil)


    assert receive_msgs(3) == [2, 1, 0]

    Process.exit(pid, :normal)

  end


  defp receive_msgs(number, res \\ [])
  defp receive_msgs(0, res), do: res
  defp receive_msgs(number, res) do
    receive do
      id -> receive_msgs(number - 1, [id | res])
    after 100 -> :error
    end
  end
end
