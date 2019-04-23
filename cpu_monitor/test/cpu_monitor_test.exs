defmodule CpuMonitorTest do
  use ExUnit.Case
  doctest CpuMonitor

  test "greets the world" do
    assert CpuMonitor.hello() == :world
  end
end
