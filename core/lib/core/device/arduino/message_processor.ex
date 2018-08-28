defmodule Device.MessageProcessor do
  @length 11
  @body_length 7
  @cmd_i 2
  @sum_i 9
  @control_bit 200




  def encode(num, cmd, args), do:
    [num, cmd | args]
    |> (&(&1 ++ [compute_control_sum(&1), @control_bit])).()
    |> (&([@control_bit, @control_bit | &1])).()

  def decode(message) do
    try do
      body = message
             |> check_head
             |> check_end
             |> check_control_sum
             |> get_body
      {:ok, body}
    rescue
      _ -> {:error, :none}
    end
  end


  ## Privates

  defp feq(x1, x2), do: x1 === x2
  defp fand(x1, x2), do: x1 and x2

  defp get_body(message), do:
    message
    |> Enum.slice(@cmd_i, @body_length)

  def sd([cmd, number | args]) do
    {}
  end

  defp check_head(message), do:
    message
    |> Enum.at(0)
    |> feq(200)
    |> fand(
         Enum.at(message, 1)
         |> feq(200)
       )
    |> check(message)

  defp check_end(message), do:
    message
    |> List.last
    |> feq(200)
    |> check(message)

  defp check_control_sum(message) do
    sum = Enum.at(message, @sum_i)
    message
    |> Enum.slice(@cmd_i, @body_length)
    |> compute_control_sum
    |> feq(sum)
    |> check(message)
  end

  defp compute_control_sum(message), do:
    message
    |> make_sum
    |> div(7)

  defp make_sum([]), do: 0
  defp make_sum([h | tail]), do: h + make_sum(tail)

  defp check(_, _, error \\ :fail)
  defp check(true, message, _), do: message
  defp check(false, _, error), do: raise error

end
