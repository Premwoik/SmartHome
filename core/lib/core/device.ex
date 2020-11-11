defmodule Core.Device do
  @moduledoc false


  defmodule BasicIO do
    @type device :: %DB.Device{}
    @type t(res) :: {:ok, res} | {:error, string()}
    @type data :: list(integer)

    @callback set_outputs(device :: %DB.Device{}, list({integer, integer})) :: t(any())
    @callback read_outputs(device :: %DB.Device{}) :: t(data)
    @callback read_inputs(device :: map) :: t(data)
    @callback heartbeat(device :: %DB.Device{}) :: t(data)
  end

  defmodule AlarmSystem do
    @type device :: %DB.Device{}
    @type t(res) :: {:ok, res} | {:error, string()}
    @type mode :: 0 | 1 | 2 | 3
    @type zones :: list(integer)
    @type commands :: list(integer)

    @callback arm(device(), mode(), zones()) :: t(any())
    @callback disarm(device(), zones()) :: t(any())
    @callback clear_alarm(device, zones()) :: t(any())
    @callback monitor_changes(device(), commands()) :: t(any())
  end

  defmodule Thermometer do
    @callback read_temperatures(device :: %DB.Device{}) :: list({list, integer})
  end

  defmodule EnergyMeter do
    @callback read_counted_values(device :: %DB.Device{}) :: list(integer)
  end

  defmodule DefaultDev do
    @callback set_time_dimmer(device :: %DB.Device{}, list({integer, integer})) :: any
  end

  alias DB.{Repo, Port}
  alias DB.DeviceJournal, as: DJ

  @type args_t :: list(%DB.Port{}) | %DB.Device{} | %{}

  @spec do_(function :: atom, args :: args_t) :: any
  def do_(function, %DB.Device{} = d) do
    execute_function(fn -> apply(module(d), function, [d]) end)
    |> DJ.log_use(d, function)
  end

  def do_(function, %{device: %DB.Device{} = d} = args) do
    execute_function(fn -> apply(module(d), function, [args]) end)
    |> DJ.log_use(d, function)
  end

  def do_(function, [%DB.Device{} = d|_] = args) do
    execute_function(fn -> apply(module(d), function, args) end)
    |> DJ.log_use(d, function)
  end

  def do_(function, ports) do
    ports
    |> handle_multiple_devices(
         fn {d, p} ->
           execute_function(fn -> apply(module(d), function, [d, p]) end, p)
         end
       )
  end

  def do_r(args, function) do
    do_(function, args)
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
        # TODO change this, becouse it looks like no data can be return 
        {:ok, []} -> :ok
        {:error, err} -> {:error, o, err}
      end
    catch
      :exit, _ ->
        {:error, o, "Device don't implement this functionality"}
    end
  end

  defp module(device) do
    device_ = DB.Repo.preload(device, :type)
    String.to_existing_atom("Elixir." <> device_.type.module)
  end
end
