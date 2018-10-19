defmodule Core.Device.Client.OneWay do
  @moduledoc false

  @behaviour Core.Devices.Client

  # TODO add send in new thread
  @impl true
  def send_msg(device, msg) do
    case send_cmd device, msg do
      {:ok, _} ->  :ok
      error -> error
    end
  end

  @impl true
  def send_with_resp(device, msg) do
    send_cmd device, msg
  end


  # Privates

  defp send_cmd(addr, command, max_attempts \\ 5)
  defp send_cmd(_, _, 0), do: {:error, "Busy too many times"}
  defp send_cmd({host,port} = addr, command, max_attempts) do
    {:ok, sock} = :gen_tcp.connect(to_charlist(host), port, [:binary, active: false], 1000)
    :ok = :gen_tcp.send sock, command
    resp = case :gen_tcp.recv sock, 23, 1000 do
      {:ok, resp} ->
        if (String.slice resp, 0..8) == "\x10\x42\x75\x73\x79\x21\x0D\x0A" do
          Process.sleep(100)
          send_cmd addr, command, max_attempts - 1
        else
          {:ok, resp}
        end
      error ->
        error
    end
    :gen_tcp.close sock
    resp
  end


end
