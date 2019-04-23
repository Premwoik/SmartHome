defmodule Core.Device.Default.Protocol do
  @length 11
  @body_length 7
  @cmd_i 2
  @sum_i 9
  @control_bit 200

  @doc """
  [control, control, num, cmd, arg0, arg1, arg2, arg3, arg4, sum, control]
  """
#TODO implement length from protocol
  def encode(num, cmd, args), do:
    [255, cmd | args] ++ [250]
#    [num, cmd | args]
#    |> (&(&1 ++ [compute_control_sum(&1), @control_bit])).()
#    |> (&([@control_bit, @control_bit | &1])).()


  def decode(message) do
    try do
      body = message
             |> check_head
             |> check_end
#             |> check_length
             |> get_body
      {:ok, body}
    rescue
      e in RuntimeError -> {:error, e.message}
    end
  end


  ## Privates

  defp feq(x1, x2), do: x1 === x2
  defp fand(x1, x2), do: x1 and x2

  defp get_body(message) do
    len = (length message) - 2
    Enum.slice(message, 1, len)
  end

  defp check_head(message), do:
    message
    |> Enum.at(0)
    |> feq(255)
    |> check(message, "wrong_header")

  defp check_end(message), do:
    message
    |> List.last
    |> feq(250)
    |> check(message, "wrong_footer")

   defp check_length(message) do
    eLen = Enum.at(message, 1)
    if eLen > 0 do
      len = (length message) - 3
      check(len = eLen, message, "wrong message length")
    end
  end

#  defp check_control_sum(message) do
#    sum = Enum.at(message, @sum_i)
#    message
#    |> Enum.slice(@cmd_i, @body_length)
#    |> compute_control_sum
#    |> feq(sum)
#    |> check(message, "invalid_control_sum")
#  end

#  defp compute_control_sum(message), do:
#    message
#    |> make_sum
#    |> div(7)
#
#  defp make_sum([]), do: 0
#  defp make_sum([h | tail]), do: h + make_sum(tail)

  defp check(_, _, error \\ :fail)
  defp check(true, message, _), do: message
  defp check(false, _, error), do: raise error

end
