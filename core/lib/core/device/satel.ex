defmodule Core.Device.Satel do
  @moduledoc false

  @behaviour Core.Device

  @client Application.get_env(:core, :one_way_client)
  @protocol Core.Devices.Satel.Protocol


  use Bitwise

  @impl true
  def set_outputs(device, ids, state) do
   {:error, :not_implement}
  end

  @impl true
  def read_active_outputs(device) do
    i_violation {device.ip, device.port}
  end

  @impl true
  def read_outputs(device) do
    {:error, :not_implement}
  end


  # Privates

  defp i_violation(addr) do
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

  defp run_command(device, cmd) do
    command = @protocol.prepare_frame cmd
    case @client.send_with_resp device, command do
      {:ok, resp} ->
        @protocol.check_resp resp
      error ->
        error
    end
  end

end
