defmodule Core.Controllers.LightControllerTest do
  @moduledoc false

  use ExUnit.Case

  alias Core.Controllers.LightController

  @device_mock Core.DeviceMock
  @db DB

  import Mox

  setup :set_mox_global
  setup :verify_on_exit!

  test "invoke turn on" do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 2, fn d, ids, state -> :ok end)

    # prepare data
    DB.Port.update_state([3, 4], false)
    DB.Dimmer.update_fill([2], 0, 1)

    # get data
    lights = DB.Light.get([3, 4])

    assert LightController.turn_on(lights) == :ok

    # verify changes in database
    [p3, p4] = DB.Port.get([3, 4])
    [d2] = DB.Dimmer.get([2])

    assert d2.fill == 100
    assert d2.direction == -1
    assert p3.state == true
    assert p4.state == true
  end

  test "invoke turn off" do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 1, fn d, ids, state -> :ok end)

    DB.Port.update_state([3, 4], true)
    DB.Dimmer.update_fill([2], 100, -1)

    # get data
    lights = DB.Light.get([3, 4])

    assert LightController.turn_off(lights) == :ok

    # verify changes in database
    [p3, p4] = DB.Port.get([3, 4])
    [d2] = DB.Dimmer.get([2])

    assert d2.fill == 0
    assert d2.direction == 1
    assert p3.state == false
    assert p4.state == false
  end
end
