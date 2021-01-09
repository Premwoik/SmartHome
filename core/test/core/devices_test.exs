defmodule Core.DeviceTest do
  use ExUnit.Case
  doctest Core
  #
  #  test "fill to time" do
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 0, time: 4_500}, 100) == 4_500
  #    assert (Core.Devices.fill_to_time %{direction: -1, fill: 100, time: 4_500}, 25) == 4_500 * 0.75
  #    assert (Core.Devices.fill_to_time %{direction: -1, fill: 50, time: 4_500}, 75) == 4_500 * 1.25
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 50, time: 4_500}, 25) == 4_500 * 1.25
  #    assert (Core.Devices.fill_to_time %{direction: -1, fill: 50, time: 4_500}, 100) == 4_500 * 1.50
  #    assert (Core.Devices.fill_to_time %{direction: -1, fill: 75, time: 4_500}, 100) == 4_500 * 1.75
  #    assert (Core.Devices.fill_to_time %{direction: -1, fill: 100, time: 4_500}, 100) == 0
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 100) == 0
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 75) == 4_500 * 0.25
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 50) == 4_500 * 0.5
  #    assert (Core.Devices.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 25) == 4_500 * 0.75
  #  end

  require Logger
  import Mox

  setup :verify_on_exit!

  defp mock(input, output, send) do
    Core.Device.DefaultTest.mock(input, output, send)
  end

  test "invoke set ports helper" do
    Core.DeviceMock
    |> expect(:set_outputs, 4, fn d, ids, state -> :ok end)

    DB.Port.update_state([12, 13, 23, 24], false)
    ports = DB.Port.get([12, 13, 23, 24])

    assert Core.Device.set_outputs_helper(ports, true) == :ok
    assert Core.Device.set_outputs_helper(ports, false) == :ok
  end

  test "invoke set ports helper - error" do
    Core.DeviceMock
    |> expect(:set_outputs, 4, fn d, ids, state -> {:error, "cant send"} end)

    DB.Port.update_state([12, 13, 23, 24], false)
    ports = DB.Port.get([12, 13, 23, 24])

    res1 = Core.Device.set_outputs_helper(ports, true)
    res2 = Core.Device.set_outputs_helper(ports, false)

    assert elem(res1, 0) == :error
    assert length(elem(res1, 1)) == 4
    assert elem(res1, 2) == ["cant send", "cant send"]
    assert elem(res2, 0) == :error
    assert length(elem(res2, 1)) == 4
    assert elem(res2, 2) == ["cant send", "cant send"]
  end

  test "invoke set ports helper - partial error" do
    Core.DeviceMock
    |> expect(
      :set_outputs,
      2,
      fn d, ids, state ->
        case d.id do
          1 -> {:error, "cant send"}
          3 -> :ok
        end
      end
    )

    DB.Port.update_state([12, 13, 23, 24], false)
    ports = DB.Port.get([12, 13, 23, 24])

    res1 = Core.Device.set_outputs_helper(ports, true)

    assert elem(res1, 0) == :error
    assert Enum.map(elem(res1, 1), & &1.id) == [12, 13]
    assert elem(res1, 2) == ["cant send"]
  end
end
