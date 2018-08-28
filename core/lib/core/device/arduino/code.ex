defmodule Device.MessageCode do
  @moduledoc false

  def write, do: 1
  def setMode, do: 2
  def synchronize, do: 3
  def writeAnalog, do: 4
  def read, do: 5
  def readAnalog, do: 6
  def test, do: 7
  def identify, do: 99

end
