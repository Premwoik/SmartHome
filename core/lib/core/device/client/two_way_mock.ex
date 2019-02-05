defmodule Core.Device.Client.TwoWayMock do
  def start_link(_, _, _, _, _, _) do
    Task.start_link(fn -> Process.sleep(99_999_999) end)
  end

  def send_with_resp(device, msg) do
    pid = self()
    Task.start_link(fn -> Process.sleep(100); send(pid, [255, 200, 200, 250]) end)
    :ok
  end

  def send_msg(device, msg) do
    :ok
  end
end
