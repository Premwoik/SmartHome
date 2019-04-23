defmodule Core.ActionsTest do
  @moduledoc false
  use ExUnit.Case

  alias Core.Controllers.BasicController
  import Mox
  import Mock

  @device_mock Core.DeviceMock


  setup :verify_on_exit!

  setup :set_mox_global

  setup do
    {:ok, pid} = Core.Actions.start_link([])
    {:ok, server: pid}
  end

  #  setup_all do
  #    DB.clear_data()
  #    DB.init_test()
  #    :ok
  #  end


  test "invoke activate_up", %{server: pid} do
    Core.Actions.ActionMock
    |> expect(
         :execute,
         fn up_down, action, s ->
           assert action.id == 3
           assert up_down == :up
           s
         end
       )
    |> expect(:init_memory, fn -> %{} end)

    assert Core.Actions.activate_up([3]) == {:ok, []}
  end

  test "invoke activate_up - wrong action id", %{server: pid} do
    assert Core.Actions.activate_up([10]) == {:ok, []}
  end

  test "invoke activate_up - too early" do
    Core.Actions.ActionMock
    |> expect(
         :execute,
         2,
         fn up_down, action, s ->
           assert up_down == :up
           s
         end
       )
    |> expect(:init_memory, 2, fn -> %{} end)

    assert Core.Actions.activate_up([1, 3, 4]) == {:ok, []}
    assert Core.Actions.activate_up([1, 3, 4]) == {
             :ok,
             [
               {4, "Its too early to invoke action again"},
               {3, "Its too early to invoke action again"}
             ]
           }
  end

  test "invoke activate_up - date - time test" do
    Core.Actions.ActionMock
    |> expect(
         :execute,
         1,
         fn up_down, action, s ->
           assert up_down == :up
           s
         end
       )
    |> expect(:init_memory, 2, fn -> %{} end)

    Core.Utils.Time.Mock
    |> expect(:now, 2,fn -> ~T[12:00:00.000000] end)
    |> expect(
         :compare,
         6,
         fn a, b ->
           cond do
             a == ~T[18:00:00.000000] && b == ~T[06:00:00.000000] -> :gt
             a == ~T[12:00:00.000000] && b == ~T[06:00:00.000000] -> :gt
             b == ~T[12:00:00.000000] && a == ~T[18:00:00.000000] -> :gt
             true -> :lt
           end
         end
       )

    assert Core.Actions.activate_up([2]) == {:ok, []}
    assert Core.Actions.activate_up([5]) == {:ok, [{5, "Its too early or too late to invoke this action"}]}

  end

end
