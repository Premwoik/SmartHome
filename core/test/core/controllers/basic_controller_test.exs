defmodule BasicControllerTest do
  @moduledoc false
  use ExUnit.Case

  alias Core.Controllers.BasicController
  require Logger

  @device_mock Core.DeviceMock
  @db DB

  import Mox

  setup :set_mox_global
  setup :verify_on_exit!

  test "invoke turn on" do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 2, fn d, ids, state -> :ok end)

    DB.Port.update_state([12, 13], false)
    ports = DB.Port.get([12, 13])

    #    {t, res} = Timex.Duration.measure(fn -> BasicController.turn_on(ports) end)
    #    Logger.info("invoke turn on :: " <> Timex.format_duration(t, :humanized))

    assert BasicController.turn_on(ports) == :ok

    # verify changes in database
    [p12, p13] = DB.Port.get([12, 13])
    assert p12.state == true
    assert p13.state == true
  end

  test "invoke turn on - error from device " do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 2, fn d, ids, state -> {:error, "err"} end)

    DB.Port.update_state([12, 13], false)
    ports = DB.Port.get([12, 13])

    #    {t, res} = Timex.Duration.measure(fn -> BasicController.turn_on(ports) end)
    #    Logger.info("invoke turn on :: " <> Timex.format_duration(t, :humanized))
    res = BasicController.turn_on(ports)

    assert elem(res, 0) == :error
    assert elem(res, 2) == ["err", "err"]

    # verify changes in database
    [p12, p13] = DB.Port.get([12, 13])
    assert p12.state == false
    assert p13.state == false
  end

  test "invoke turn off - ports are off" do
    #      TODO check this later
    # zero because ports state is false
    @device_mock
    |> expect(:set_outputs, 0, fn d, ids, state -> :ok end)

    DB.Port.update_state([12, 13], false)
    ports = [p12_, p13_] = DB.Port.get([12, 13])

    #      {t, res} = Timex.Duration.measure(fn -> BasicController.turn_off(ports) end)
    #      Logger.info("invoke turn off - ports are off :: " <> Timex.format_duration(t, :humanized))

    assert BasicController.turn_off(ports) == :ok

    # verify changes in database
    [p12, p13] = DB.Port.get([12, 13])
    assert p12.state == false
    assert p13.state == false
  end

  test "invoke turn off" do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 2, fn d, ids, state -> :ok end)

    DB.Port.update_state([12, 13], true)
    ports = DB.Port.get([12, 13])

    #      {t, res} = Timex.Duration.measure(fn -> BasicController.turn_off(ports) end)
    #      Logger.info("invoke turn off :: " <> Timex.format_duration(t, :humanized))
    #
    assert BasicController.turn_off(ports) == :ok

    # verify changes in database
    [p12, p13] = DB.Port.get([12, 13])
    assert p12.state == false
    assert p13.state == false
  end

  test "invoke toggle" do
    # second execution is for pulse port
    @device_mock
    |> expect(:set_outputs, 2, fn d, ids, state -> :ok end)

    DB.Port.update_state([12], true)
    DB.Port.update_state([13], false)

    ports = DB.Port.get([12, 13])

    #      {t, res} = Timex.Duration.measure(fn -> BasicController.toggle(ports) end)
    #      Logger.info("invoke toggle :: " <> Timex.format_duration(t, :humanized))

    assert BasicController.toggle(ports) == :ok

    # verify changes in database
    [p12, p13] = DB.Port.get([12, 13])
    assert p12.state == false
    assert p13.state == true
  end
end
