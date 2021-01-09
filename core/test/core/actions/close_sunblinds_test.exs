defmodule CloseSunblindsTest do
  @moduledoc false

  use ExUnit.Case

  alias Core.Controllers.BasicController
  import Mox

  @device_mock Core.DeviceMock

  setup :verify_on_exit!

  test "different signals" do
    Core.DeviceMock
    |> expect(:set_outputs, 2, fn d, ids, state -> :ok end)

    DB.Sunblind.all()
    |> DB.Sunblind.update_state("open")

    assert Core.Actions.CloseSunblinds.execute(:up, nil, :ok) == :ok
    assert Enum.all?(DB.Sunblind.all(), fn s -> s.state == "in_move" end) == true

    assert Core.Actions.CloseSunblinds.execute(:down, nil, :ok) == :ok
    assert Enum.all?(DB.Sunblind.all(), fn s -> s.state == "in_move" end) == true
  end
end
