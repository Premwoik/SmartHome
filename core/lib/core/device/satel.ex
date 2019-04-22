defmodule Core.Device.Satel do
  @moduledoc false

  @behaviour Core.Device

  @client Application.get_env(:core, :two_way_client)
  # @client Core.Device.Client.OneWay
  @protocol Core.Devices.Satel.Protocol

  alias DB.DeviceJournal

  use Bitwise

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    @client.start_link(host, port, [:binary], keywords, timeout, length)
  end

  @impl true
  def set_outputs(device, ids, state) do
    {:error, :not_implement}
  end

  @impl true
  def read_active_inputs(device) do
    DeviceJournal.log(device.id, "read_active_inputs", info = "")
    i_violation(device)
  end

  @impl true
  def read_outputs(device) do
    {:error, :not_implement}
  end

  # Privates

  defp i_violation(device) do
    case run_command(device, "00") do
      {:ok, resp} ->
        :erlang.binary_to_list(resp)
        |> unpack_bytes
        |> (fn x -> {:ok, x} end).()

      error ->
        error
    end
  end

  defp unpack_bytes(data, i \\ 0, res \\ [])
  defp unpack_bytes(_, 8, res), do: res

  defp unpack_bytes([h | t], i, res) do
    unpack_bytes(t, i + 1, unpack_byte(h, i, res))
  end

  defp unpack_byte(h, i, res, j \\ 0)
  defp unpack_byte(_, _, res, 8), do: res

  defp unpack_byte(h, i, res, j) do
    if (:math.pow(2, j) |> round &&& h) != 0 do
      unpack_byte(h, i, [8 * i + j + 1 | res], j + 1)
    else
      unpack_byte(h, i, res, j + 1)
    end
  end

  defp run_command(device, cmd) do
    with command <- @protocol.prepare_frame(cmd),
         :ok <- @client.send_with_resp(device, command),
         {:ok, resp} <- wait_for_confirmation(2_000) do
      @protocol.check_resp(resp)
    else
      err -> err
    end
  end

  defp wait_for_confirmation(timeout) do
    receive do
      resp ->
        if String.slice(resp, 0..8) == "\x10\x42\x75\x73\x79\x21\x0D\x0A" do
          # Process.sleep(100)
          # send_cmd(addr, command, max_attempts - 1)
          {:error, :busy}
        else
          {:ok, resp}
        end
    after
      timeout ->
        {:error, :timeout}
    end
  end
end
