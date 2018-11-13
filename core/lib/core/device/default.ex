defmodule Core.Device.Default do
  @moduledoc false
  require Logger
  use Bitwise

  defmodule Code do
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

  @behaviour Core.Device

  @client Application.get_env(:core, :two_way_client)
  @protocol Core.Device.Default.Protocol

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    @client.start_link(host, port, opts, keywords, timeout, length)
  end

  @impl true
  def read_active_outputs(device) do
    :not_implement
  end


  @impl true
  def read_outputs(device) do
    :not_implement
  end

  @impl true
  def set_outputs(device, pins, state) do
    Logger.info("#{device.name} - set_outputs - #{inspect pins}")
    cmd = Code.write
    args = [bool_to_bin(!state) | pins_to_bytes(pins)]
    noreply_send(device, cmd, args)
  end

  defp synchronize_outputs(device, pins) do
    cmd = Code.synchronize
    args = [0 | pins_to_bytes(pins)]
    noreply_send(device, cmd, args)
  end

  defp read_input(device, pin) do
    cmd = Code.read
    args = [pin, 0, 0, 0, 0]
  end

  defp read_analog(device, pin) do
    cmd = Code.readAnalog
    args = [pin, 0, 0, 0, 0]
  end

  defp write_analog(device, pin) do
    cmd = Code.writeAnalog
    args = [pin, 0, 0, 0, 0]
  end

  defp test(device) do
    cmd = Code.test
    args = [0, 0, 0, 0, 0]
    noreply_send(device, cmd, args)
  end

  # Privates

  defp bool_to_bin(true), do: 1
  defp bool_to_bin(false), do: 0


  defp pins_to_bytes([]), do: [0, 0, 0, 0]
  defp pins_to_bytes([pin | rest]) do
    res = pin - 18
    byte_index = div res, 8
    bit_index = rem res, 8

    bytes = pins_to_bytes(rest)
    new_byte = bytes
              |> Enum.at(byte_index)
              |> bor(bsl 1, bit_index)
    List.replace_at(bytes, byte_index, new_byte)
  end


  defp noreply_send(device, cmd, args) do
    msg = @protocol.encode(0, cmd, args)
    case @client.send_with_resp device, msg do
      :ok -> wait_for_confirmation()
      error -> error
    end
  end

  defp wait_for_confirmation(timeout \\ 1_000) do
    receive do
    msg ->
        case @protocol.decode msg do
#          {:error, _} -> Logger.error "Decoding message error"; IO.inspect msg;  false
          {:ok, [num, cmd | args]} ->
            case cmd == 100 do
              true -> :ok
              false -> {:error, "wrong response code"}
            end
          error -> error
        end
    after
      timeout -> {:error, "confirmation timeout"}
    end
  end
end
