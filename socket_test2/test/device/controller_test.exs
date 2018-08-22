defmodule Device.ControllerTest do
  use ExUnit.Case
  doctest SocketTest2

  test "fill to time" do
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 0, time: 4_500}, 100) == 4_500
    assert (Device.Controller.fill_to_time %{direction: -1, fill: 100, time: 4_500}, 25) == 4_500 * 0.75
    assert (Device.Controller.fill_to_time %{direction: -1, fill: 50, time: 4_500}, 75) == 4_500 * 1.25
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 50, time: 4_500}, 25) == 4_500 * 1.25
    assert (Device.Controller.fill_to_time %{direction: -1, fill: 50, time: 4_500}, 100) == 4_500 * 1.50
    assert (Device.Controller.fill_to_time %{direction: -1, fill: 75, time: 4_500}, 100) == 4_500 * 1.75
    assert (Device.Controller.fill_to_time %{direction: -1, fill: 100, time: 4_500}, 100) == 0
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 100) == 0
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 75) == 4_500 * 0.25
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 50) == 4_500 * 0.5
    assert (Device.Controller.fill_to_time %{direction: 1, fill: 100, time: 4_500}, 25) == 4_500 * 0.75
  end

end
