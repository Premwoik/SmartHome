defmodule Core.Device do
  @moduledoc false

  @callback set_outputs(device :: %DB.Device{}, list({integer, integer})) :: any
  @callback set_pwm_outputs(device :: %DB.Device{}, list({integer, integer})) :: any
  @callback read_outputs(device :: %DB.Device{}) :: list({integer, boolean})
  @callback read_active_inputs(device :: map) :: list(integer)
  @callback read_temperatures(device :: %DB.Device{}) :: list({list, integer})
  @callback read_counted_values(device :: %DB.Device{}) :: list(integer)
  @callback heartbeat(device :: %DB.Device{}) :: any

  alias DB.Port

  @type args_t :: list(%DB.Port{}) | %DB.Device{}

  @spec do_(function :: atom, args :: args_t) :: any
  def do_(function, %DB.Device{} = d) do
    execute_function(fn -> apply(module(d), function, [d]) end)
  end

  def do_(function, ports) do
    ports
    |> handle_multiple_devices(fn {d, p} ->
      execute_function(fn -> apply(module(d), function, [d, p]) end, p)
    end)
  end

  def do_r(args, function) do
    do_(function, args)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def set_outputs_helper(ports, state) do
    ports
    |> Enum.map(fn p -> %{p | state: state} end)
    |> handle_multiple_devices(fn {d, p} ->
      execute_function(fn -> module(d).set_outputs(d, ports_to_num(p)) end, p)
    end)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def set_pwm_outputs(ports) do
    ports
    |> handle_multiple_devices(fn {d, p} ->
      arg = Enum.map(p, fn p_ -> {p_.number, p_.pwm_fill} end)
      execute_function(fn -> module(d).set_pwm_outputs(d, arg) end, p)
    end)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def read_active_inputs(device) do
    execute_function(fn -> module(device).read_active_inputs(device) end)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def read_temperatures(device) do
    execute_function(fn -> module(device).read_temperatures(device) end)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def read_counted_values(device) do
    execute_function(fn -> module(device).read_counted_values(device) end)
  end

  @deprecated "use do_/2 or do_r/2 instead"
  def send_heartbeat(device) do
    execute_function(fn -> module(device).heartbeat(device) end)
  end

  # Privates

  defp handle_multiple_devices(ports, func) do
    ports
    |> Enum.group_by(& &1.device)
    |> Enum.map(func)
    |> Enum.filter(fn x -> x != :ok end)
    |> case do
      [] ->
        :ok

      errors ->
        errors
        |> Enum.reduce(
          {:error, [], []},
          fn {:error, p1, err}, {:error, p2, err_list} ->
            {:error, p1 ++ p2, [err | err_list]}
          end
        )
    end
  end

  defp execute_function(func, o \\ nil)

  defp execute_function(func, nil) do
    try do
      func.()
    catch
      :exit, _ ->
        {:error, "Device don't implement this functionality"}
    end
  end

  defp execute_function(func, o) do
    try do
      case func.() do
        {:ok, []} -> :ok
        {:error, err} -> {:error, o, err}
      end
    catch
      :exit, _ ->
        {:error, o, "Device don't implement this functionality"}
    end
  end

  defp build_pwm_fun_args(ports) do
  end

  defp ports_to_num(ports) do
    ports
    |> Enum.flat_map(fn p ->
      if p.inverted_logic,
        do: [p.number, state_to_num(not p.state)],
        else: [p.number, state_to_num(p.state)]
    end)
    |> IO.inspect()
  end

  defp state_to_num(false), do: 0
  defp state_to_num(_), do: 1

  defp module(device) do
    String.to_existing_atom("Elixir." <> device.type)
  end
end
