defmodule Core.Controllers.BasicController do
  @moduledoc """

    API:
    - turn on
    - turn off
    - toggle
    -

  """
  alias DB.{Device, Port}

  @callback set_pwm_outputs(device :: %DB.Device{}, ports :: list(%DB.Port{})) :: any
  @callback set_outputs(device :: %DB.Device{}, ports :: list(%DB.Port{})) :: any
  @callback read_active_inputs(device :: %DB.Device{}) :: any

  @doc "ports "
  def turn_on(ports, pid \\ nil) do
    ports
    |> set(true, pid)
  end

  def turn_off(ports, pid \\ nil) do
    ports
    |> set(false, pid)
  end

  def set(ports, state, pid \\ nil) do
    ports
    |> Enum.split_with(&(&1.timeout > 0))
    |> (fn {pulse, normal} ->
          [set_normal_ports(normal, state), set_pulse_ports(pulse, state, pid)]
        end).()
    |> flatten_result()
    |> case do
      :ok ->
        DB.Port.update(ports, state: state)
        :ok

      {:error, p, _} = res ->
        filter_invalid(ports, p)
        |> DB.Port.update(state: state)

        res
    end
  end

  def impulse(ports, pid \\ nil) do
    with :ok <- set_pulse_ports(ports, nil, pid) do
      ports
      |> Enum.map(& &1.id)
      |> Port.update_state(nil)

      :ok
    end
  end

  def pwm(ports, fill) do
    state = if fill > 0, do: true, else: false

    Enum.map(ports, fn p -> %{p | state: state, pwm_fill: fill} end)
    |> Core.Device.do_r(:set_pwm_outputs)
    |> case do
      :ok ->
        DB.Port.update(ports, state: state, pwm_fill: fill)
        :ok

      {:error, p, _} = res ->
        filter_invalid(ports, p)
        |> DB.Port.update(state: state, pwm_fill: fill)

        res
    end
  end

  def read(%DB.Device{} = d) do
    Core.Device.do_(:read_active_inputs, d)
  end

  def flatten_result(list) do
    list
    |> Enum.filter(&(&1 != :ok))
    |> case do
      [] ->
        :ok

      list_ ->
        Enum.reduce(list_, fn {:error, p1, err1}, {:error, p2, err2} ->
          {:error, p1 ++ p2, err1 ++ err2}
        end)
    end
  end

  def prepare_for_basic(any) when is_list(any) do
    any
    |> Enum.map(& &1.port)
    |> DB.Repo.preload(:device)
  end

  def prepare_for_basic(any) do
    any
    |> DB.Repo.preload(:device)
  end

  def recv_postpone_resp(timeout \\ 10_000) do
    receive do
      {:pulse_result, msg} -> msg
    after
      timeout -> {:error, "Waiting for postpone timeout"}
    end
  end

  @spec postpone_turn_off(list(integer), integer()) :: any()
  def postpone_turn_off(ports, timeouts, resp_pid \\ nil)

  def postpone_turn_off(ports, timeout, resp_pid) do
    receive do
    after
      timeout ->
        resp_msg = set_normal_ports(ports, false)

        case resp_pid do
          nil -> :ok
          pid -> send(pid, {:pulse_result, resp_msg})
        end
    end
  end

  # Privates

  @spec set_normal_ports(list(map()), boolean()) :: {ok :: list(map), error :: list}
  defp set_normal_ports([], _), do: :ok

  defp set_normal_ports(ports, state) do
    ports
    |> Enum.map(fn p -> %{p | state: state} end)
    |> Core.Device.do_r(:set_outputs)
  end

  @spec set_pulse_ports(list(map()), boolean(), pid()) :: any
  defp set_pulse_ports([], _, pid), do: :ok

  defp set_pulse_ports(ports, _, pid) do
    res = set_normal_ports(ports, true)

    case res do
      :ok ->
        ports

      {:error, p, _} ->
        filter_invalid(ports, p)
    end
    |> Enum.group_by(& &1.timeout)
    |> Map.to_list()
    |> Enum.each(fn {k, v} -> Task.start(fn -> postpone_turn_off(v, k, pid) end) end)

    res
  end

  defp filter_invalid(ports, invalid) do
    Enum.filter(ports, fn x -> !Enum.any?(invalid, fn x1 -> x1 == x end) end)
  end

  defp group_by_device(ports) do
    Enum.group_by(ports, fn port -> port.device end)
  end
end
