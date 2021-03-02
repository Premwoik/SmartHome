defmodule Core.Controllers.SunblindControllerTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Core.Controllers.SunblindController

  @device_mock Core.DeviceMock

  import Mox

  setup :verify_on_exit!

  test "invoke close - check only 'only_close'" do
    @device_mock
    |> expect(:set_outputs, 1, fn d, ids, state -> :ok end)

    DB.Port.update_state([13, 14], false)
    DB.Sunblind.update_state([13, 14], "open")
    sunblinds = DB.Sunblind.get([13, 14])

    assert SunblindController.close(sunblinds) == :ok

    [s13, s14] = DB.Sunblind.get([13, 14])

    assert s13.port.state == true
    assert s13.state == "in_move"
    assert s14.port.state == true
    assert s14.state == "in_move"
  end

  test "invoke open - check only 'only_close'" do
    @device_mock
    |> expect(:set_outputs, 1, fn d, ids, state -> :ok end)

    DB.Port.update_state([13, 14], true)
    DB.Sunblind.update_state([13, 14], "close")
    sunblinds = DB.Sunblind.get([13, 14])

    assert SunblindController.open(sunblinds) == :ok

    [s13, s14] = DB.Sunblind.get([13, 14])

    assert s13.port.state == false
    assert s13.state == "in_move"
    assert s14.port.state == false
    assert s14.state == "in_move"
    :ok
  end
end
