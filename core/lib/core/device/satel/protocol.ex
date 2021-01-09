defmodule Core.Device.Satel.Protocol do
  @moduledoc false

  use Bitwise
  require Logger

  @header "\xFE\xFE"
  @footer "\xFE\x0D"

  def encode_cmd(cmd) do
    :binary.encode_unsigned(cmd, :big)
  end

  @spec decode_commands_status(binary(), list(integer())) :: list(integer())
  def decode_commands_status(outputs, monitored_commands) do
    os = :binary.decode_unsigned(outputs, :little)
    Enum.map(monitored_commands, fn cmd -> is_output_active(cmd + 1, os) end)
  end

  @spec encode_outputs(list(integer()), integer()) :: binary()
  def encode_outputs(outputs, len \\ 16) do
    outputs_ =
      Enum.reduce(outputs, fn o, acc -> 1 <<< (o - 1) ||| acc end)
      |> :binary.encode_unsigned(:little)

    s = (len - :erlang.size(outputs_)) * 8
    outputs_ <> <<0x00::size(s)>>
  end

  @spec decode_outputs(binary(), integer()) :: list(integer())
  def decode_outputs(outputs, num \\ 16) do
    os = :binary.decode_unsigned(outputs, :little)

    Enum.reduce(1..(num * 8), [], fn o, acc ->
      if(is_output_active(o, os), do: [o | acc], else: acc)
    end)
  end

  defp is_output_active(o, outputs) do
    (1 <<< (o - 1) &&& outputs) > 0
  end

  @spec encode_code(String.t()) :: binary()
  def encode_code(code) do
    code = Base.decode16!(code)
    s = 8 - :erlang.size(code)
    suffix = :binary.list_to_bin(List.duplicate(0xFF, s))
    code <> suffix
  end

  @spec prepare_frame(binary()) :: binary()
  def prepare_frame(command) do
    checksum_ =
      checksum(command)
      |> :binary.encode_unsigned(:big)

    body =
      (command <> checksum_)
      |> :binary.replace(<<0xFE>>, <<0xFE, 0xF0>>, [:global])

    @header <> body <> @footer
  end

  @spec check_response(binary(), binary()) :: binary() | {:error, String.t()}
  def check_response(resp, cmd) do
    Logger.log(:debug, "Integra response: #{inspect(resp, base: :hex)}")

    with resp_ <- :binary.replace(resp, <<0xFE, 0xF0>>, <<0xFE>>, [:global]),
         {:ok, r1} <- check_header(resp_),
         {:ok, r2} <- check_footer(r1),
         {:ok, r4} <- verify_checksum(r2),
         {:ok, to_skip} <- verify_response_code(r2, cmd) do
      {:ok, if(to_skip > 0, do: :binary.part(r4, to_skip, :erlang.size(r4) - to_skip), else: r4)}
    end
  end

  # Privates

  defp check_header(<<header::binary-size(2), tail::binary>>) do
    if(header == @header, do: {:ok, tail}, else: {:error, "Wrong header"})
  end

  defp check_footer(resp) do
    s = :erlang.size(resp) - 2
    <<first::binary-size(s), footer::binary>> = resp
    if(footer == @footer, do: {:ok, first}, else: {:error, "Wrong footer"})
  end

  defp verify_response_code("\xEF" <> <<h::binary-size(1), _::binary>>, _) do
    if h in ["\xFF", "\x00"] do
      {:error, "Integra reported an error code"}
    else
      {:ok, 2}
    end
  end

  defp verify_response_code(<<h::binary-size(1), _::binary>>, cmd) when h == cmd, do: {:ok, 1}
  defp verify_response_code(_, _), do: {:error, "Response cmd does not match request"}

  defp verify_checksum(output) do
    s = :erlang.size(output) - 2
    <<body::binary-size(s), sum::binary>> = output
    calcRespSum = body |> checksum
    extrRespSum = sum |> :binary.decode_unsigned()
    Logger.debug("Calculated checksum: #{calcRespSum}, received checksum: #{extrRespSum}")

    if calcRespSum != extrRespSum do
      {:error, "Wrong checksum"}
    else
      {:ok, body}
    end
  end

  defp checksum(command) do
    byte_array = :binary.bin_to_list(command)

    Enum.reduce(byte_array, 0x147A, fn b, crc ->
      crc = (crc <<< 1 &&& 0xFFFF) ||| (crc &&& 0x8000) >>> 15
      crc = crc ^^^ 0xFFFF
      crc + (crc >>> 8) + b &&& 0xFFFF
    end)
  end
end
