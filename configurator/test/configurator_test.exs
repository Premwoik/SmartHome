defmodule ConfiguratorTest do
  use ExUnit.Case
  doctest Configurator

  test "greets the world" do
    assert Configurator.hello() == :world
  end
end
