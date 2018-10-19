defmodule Core.Devices.Satel.Protocol do
  @moduledoc false

 use Bitwise

  @header "\xFE\xFE"
  @footer "\xFE\x0D"

  def prepare_frame(command) do
    data = command
           |> Base.decode16!
           |> to_charlist

    c = checksum data

    data ++ [c >>> 8, c &&& 0xFF]
    |> :erlang.list_to_binary
    |> (String.replace "\xFE", "\xFE\xF0")
    |> fn data -> @header <> data <> @footer end.()

  end

  def check_resp(resp) do
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

  # Privates

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
