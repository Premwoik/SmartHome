defmodule SatelProtocol do
  @moduledoc false

 use Bitwise

  @header "\xFE\xFE"
  @footer "\xFE\x0D"


  @doc """
    iViolation
  """
  def iviolation(addr) do
    case run_command(addr, "00") do
      {:ok, resp} ->
        (:erlang.binary_to_list resp)
        |> unpack_bytes
        |> fn x -> {:ok, x} end.()
      error ->
        error
    end
  end

  defp unpack_bytes(data, i \\0, res \\ [])
  defp unpack_bytes(_, 8, res), do: res
  defp unpack_bytes([h | t], i, res) do
    unpack_bytes t, i + 1, (unpack_byte h, i, res)
  end

  defp unpack_byte(h, i, res, j \\0)
  defp unpack_byte(_,_, res, 8), do: res
  defp unpack_byte(h, i, res, j) do
    if (((:math.pow 2, j) |> round) &&& h) != 0 do
      unpack_byte(h,i,[8*i+j+1 | res], j+1)
    else
      unpack_byte(h,i, res, j+1)
    end
  end

  @doc """
    runCommand
  """
  def run_command(addr, cmd) do
    command = prepare_frame cmd
    case send_cmd addr, command do
      {:ok, resp} ->
        check_resp resp
      error ->
        error
    end
  end

  ## Privates

  defp send_cmd(addr, command, maxAttempts \\ 5)
  defp send_cmd(_, _, 0), do: {:error, "Busy too many times"}
  defp send_cmd({host,port}=addr, command, maxAttempts) do
    {:ok, sock} = :gen_tcp.connect host, port, [:binary, active: false], 1000
    :ok = :gen_tcp.send sock, command
    resp = case :gen_tcp.recv sock, 23, 1000 do
      {:ok, resp} ->
        if (String.slice resp, 0..8) == "\x10\x42\x75\x73\x79\x21\x0D\x0A" do
          Process.sleep(1000)
          send_cmd addr, command, maxAttempts-1
        else
          {:ok, resp}
        end
      error ->
        error
    end
    :gen_tcp.close sock
    resp
  end

  defp check_resp(resp) do
    cond do
      (String.slice resp, 0..1) != @header -> {:error, "Wrong header"}
      (String.slice resp, -2..-1) != @footer -> {:error, "Wrong footer"}
      true ->
        output = (String.slice resp, 2..-3) |> (String.replace "\xFE\xF0", "\xFE")
        cond do
          (String.first output) == "\xEF" ->
            if (Enum.any? ["\xFF", "\x00"], &(&1 == String.at output, 1)) do
              {:error, "Integra reported an error code"}
            else
              check_sum(output)
            end
#          (String.first output) != String.at resp, 2 ->
#            {:error, "Response to a wrong command"}
          true -> check_sum(output)
        end
    end
  end

  defp check_sum(output) do
    get = fn r -> (String.at output, r) |> :erlang.binary_to_list |> List.first end
    calcRespSum = (String.slice output, 0..-3) |> :erlang.binary_to_list |> checksum
    extrRespSum = 256 * get.(-2) + get.(-1)
    if calcRespSum != extrRespSum do
      {:error, "Wrong checksum"}
    else
      {:ok, (String.slice output, 1..-3)}
    end
  end

  defp prepare_frame(command) do
    data = command
           |> Base.decode16!
           |> to_charlist

    c = checksum data

    data ++ [c >>> 8, c &&& 0xFF]
    |> :erlang.list_to_binary
    |> (String.replace "\xFE", "\xFE\xF0")
    |> fn data -> @header <> data <> @footer end.()

  end

  defp checksum(command, crc \\ 0x147A)
  defp checksum([], crc), do: crc
  defp checksum([byte | tail], crc) do
    crc
    |> (&(((&1 <<< 1) &&& 0xFFFF) ||| (&1 &&& 0x8000) >>> 15)).()
    |> (bxor 0xFFFF)
    |> (&(&1 + (&1 >>> 8) + byte)).()
    |> (band 0xFFFF)
    |> (&checksum tail, &1).()
  end

end
