defmodule Device.Command do
  @moduledoc false
  alias Device.MessageCode, as: Code

  require Logger
  use Bitwise

  def set_output_state(device, pins, false = state), do: set_output device, pins
  def set_output_state(device, pins, true = state), do: reset_output device, pins


  def set_output(device, pins) do
    cmd = Code.write
    args = [1 | pins_to_bytes(pins)]
    noreply_send(device, cmd, args)
  end

  def reset_output(device, pins) do
    cmd = Code.write
    args = [0 | pins_to_bytes(pins)]
    noreply_send(device, cmd, args)
  end

  def synchronize_outputs(device, pins) do
    cmd = Code.synchronize
    args = [0 | pins_to_bytes(pins)]
    noreply_send(device, cmd, args)
  end

  def read_input(device, pin) do
    cmd = Code.read
    args = [pin, 0, 0, 0, 0]
  end

  def read_analog(device, pin) do
    cmd = Code.readAnalog
    args = [pin, 0, 0, 0, 0]
  end

  def write_analog(device, pin) do
    cmd = Code.writeAnalog
    args = [pin, 0, 0, 0, 0]
  end

  def test(device) do
    cmd = Code.test
    args = [0, 0, 0, 0, 0]
    noreply_send(device, cmd, args)
  end

  #  ---privates
  defp pins_to_bytes([]), do: [0, 0, 0, 0]
  defp pins_to_bytes([pin | rest]) do
    res = pin - 18
    byteIndex = div res, 8
    bitIndex = rem res, 8

    bytes = pins_to_bytes(rest)
    newByte = bytes
              |> Enum.at(byteIndex)
              |> bor(bsl 1, bitIndex)
    List.replace_at(bytes, byteIndex, newByte)
  end


  defp noreply_send(device, cmd, args) do
    msg = Device.MessageProcessor.encode(0, cmd, args)
    case Device.Client.confirmed_send device, msg do
      :ok -> wait_for_confirmation()
      _ -> false
    end
  end

  defp wait_for_confirmation() do
    receive do
    {:msg, msg} ->
        case Device.MessageProcessor.decode msg do
          {:error, _} -> Logger.error "Decoding message error"; IO.inspect msg;  false
          {:ok, [num, cmd | args]} -> cmd == 100
        end
    after
      10_000 -> false
    end
  end
end
