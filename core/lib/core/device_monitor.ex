defmodule Core.DeviceMonitor do
  @moduledoc false

  def disconnected(device) do
    :ok
  end

  def connected(device) do
    :ok
  end
  
  def connection_error(device, error) do
    :ok
  end

  def check_journal(device) do
    :ok 
  end

end
