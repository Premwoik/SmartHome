defmodule Core.Controllers.BasicController do
  @moduledoc """

    API:
    - turn on
    - turn off
    - toggle
    -

  """
  alias DB.{Device, Port}

  @behaviour Core.Controllers.Controller

  def turn_on(ports, pid \\ nil) do
    ports
    |> set_ports(true, pid)
  end

  def turn_off(ports, pid \\ nil) do
    ports
    |> set_ports(false, pid)
  end

  def toggle(ports, pid \\ nil) do
    ports
    |> Enum.split_with(fn port -> port.state end)
    |> fn {t, f} -> [set_ports(t, false, pid), set_ports(f, true, pid)] end.()
    |> flatten_result()
  end

  def impulse(ports, pid \\ nil) do
    with :ok <- set_pulse_ports(ports, nil, pid) do
      ports
      |> Enum.map(&(&1.id))
      |> Port.update_state(nil)
    end
  end

  def flatten_result(list) do
    list
    |> Enum.filter(&(&1 != :ok))
    |> case do
         [] -> :ok
         list_ -> Enum.reduce(list_, fn {:error, p1, err1}, {:error, p2, err2} -> {:error, p1 ++ p2, err1 ++ err2}  end)

       end
  end

  def prepare_for_basic(any) do
    any
    |> Enum.map(&(&1.port))
    |> DB.Repo.preload(:device)
  end


  def recv_postpone_resp(timeout \\ 10_000) do
    receive do
      {:pulse_result, msg} -> msg
    after timeout -> {:error, "Waiting for postpone timeout"}
    end
  end

  @spec postpone_turn_off(list(integer), integer()) :: any()
  def postpone_turn_off(ports, timeouts, resp_pid \\ nil)
  def postpone_turn_off(ports, timeout, resp_pid) do
    receive do
    after
      timeout ->
        resp_msg = Core.Device.set_outputs_helper(ports, false)
        case resp_pid do
          nil -> :ok
          pid -> send pid, {:pulse_result, resp_msg}
        end
    end
  end

  # Privates

  defp set_ports(ports, state, pid) do
    ports
    # TODO decide what to do with bottom line
    #    |> Enum.filter(&(&1.state != state))
    |> Enum.split_with(&(&1.timeout > 0))
    |> fn {pulse, normal} ->
      [set_normal_ports(normal, state), set_pulse_ports(pulse, state, pid)]
       end.()
    |> flatten_result()
    |> case do
         :ok ->
           save(ports, state)
           :ok
         {:error, p, _} = res ->
           filter_invalid(ports, p)
           |> save(state)
           res
       end
  end

  defp save(ports, state) do
    Enum.map(ports, &(&1.id))
    |> Port.update_state(state)
  end

  @spec set_normal_ports(list(map()), boolean()) :: {ok :: list(map), error :: list}
  defp set_normal_ports([], _), do: :ok
  defp set_normal_ports(ports, state) do
    Core.Device.set_outputs_helper(ports, state)
  end



  @spec set_pulse_ports(list(map()), boolean(), pid()) :: any
  defp set_pulse_ports([], _, pid), do: :ok
  defp set_pulse_ports(ports, _, pid) do
    res = set_normal_ports ports, true

    case res do
      :ok -> ports
      {:error, p, _} ->
        filter_invalid(ports, p)
    end
    |> Enum.group_by(&(&1.timeout))
    |> Map.to_list()
    |> (Enum.each fn {k, v} -> Task.start fn -> postpone_turn_off v, k, pid end end)

    res
  end

  defp filter_invalid(ports, invalid) do
    Enum.filter(ports, fn x -> !Enum.any? invalid, fn x1 -> x1 == x end end)
  end

  defp group_by_device(ports) do
    Enum.group_by(ports, fn port -> port.device end)
  end

end
