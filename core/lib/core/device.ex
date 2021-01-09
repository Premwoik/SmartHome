defmodule Core.Device do
  @moduledoc false

  @callback start_link(
              ip :: String.t(),
              port :: integer(),
              opts :: any(),
              keywords :: any(),
              timeout :: integer,
              length :: integer
            ) :: bool()

  defmodule BasicIO do
    alias DB.{Device, Port}
    @type t(res) :: {:ok, res} | {:error, String.t()}
    @type def_t :: :ok | {:error, String.t()}
    @type active_ports :: [integer]

    @doc """
      Sets the device's outputs.

      Returns :ok if instruction was executed correctly.
    """
    @callback set_outputs(%Device{}, [%DB.Port{}]) :: def_t

    @doc """
      Reads the device's outputs.

      Returns list of the actually active outputs.
    """
    @callback read_outputs(%Device{}) :: t(active_ports)

    @doc """
      Reads the device's inputs.

      Returns list of the actually active inputs.
    """
    @callback read_inputs(%Device{}) :: t(active_ports)

    @doc """
      Sends heartbeat message to the device.

      Returns :ok if receive response from device.
    """
    @callback heartbeat(%Device{}) :: def_t
  end

  defmodule PwmOutputs do
    @doc """
      Sets pwm outputs

    """
    @callback set_pwm_outputs(%DB.Device{}, [%DB.Port{}]) :: any
  end

  defmodule AlarmSystem do
    @type device :: %DB.Device{}
    @type t(res) :: {:ok, res} | {:error, String.t()}
    @type mode :: 0 | 1 | 2 | 3
    @type zones :: [integer]
    @type commands :: [integer]

    @callback arm(device(), mode(), zones()) :: t(any())
    @callback disarm(device(), zones()) :: t(any())
    @callback clear_alarm(device, zones()) :: t(any())
    @callback monitor_changes(device(), commands()) :: t(any())
  end

  defmodule RgbW do
    alias DB.{Device, Dimmer}
    @type result :: {:ok, %Dimmer{}} | {:error, String.t()}

    @callback set_state(%Dimmer{}) :: result
    @callback set_brightness(%Dimmer{}) :: result
    @callback set_white_brightness(%Dimmer{}) :: result
    @callback set_color(%Dimmer{}) :: result
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

  alias DB.DeviceJournal, as: DJ
  alias DB.{Device, Port}
  import Core.Controllers.Universal, only: [flatten_result: 1]

  @type args_t ::
          [%Port{}]
          | [any]

  @doc """
  Calls the device's function.

  The

  ## Parameters

    - function: Atom that represents function name.
    - args: Structure that contains argument that will be passed to function.

  ## Examples

    device = DB.Repo.get(DB.Device, 1)
    Core.Device.do_(:read_inputs, device)

  """
  @spec do_(function :: atom, args :: args_t) :: any

  def do_(function, [%Port{} | _] = ports) do
    ports
    |> handle_multiple_devices(fn {d, p} ->
      execute_function(fn -> apply(module(d), function, [d, p]) end, p)
      |> DJ.log_use(d, function, p)
    end)
  end

  def do_(function, args) do
    d = get_device(args)
    args = List.wrap(args)

    execute_function(fn -> apply(module(d), function, args) end)
    |> DJ.log_use(d, function, args)
  end

  def do_r(args, function) do
    do_(function, args)
  end

  # Privates

  defp get_device(args) when is_list(args) do
    Enum.find(args, fn item ->
      with false <- item == %Device{},
           false <- Map.has_key?(item, :device),
           true <- Map.has_key?(item, :port),
           do: Map.has_key?(item.port, :device)
    end)
    |> get_device()
  end

  defp get_device(arg) do
    case arg do
      %Device{} -> arg
      nil -> {:error, "Cannot find device!"}
      %{device: d} -> d
      %{port: %{device: d}} -> d
    end
  end

  defp handle_multiple_devices(ports, func) do
    ports
    |> Enum.group_by(& &1.device)
    |> Enum.map(func)
    |> flatten_result()
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
        # TODO change this, because it looks like no data can be return
        {:error, err} -> {:error, o, err}
        ok -> ok
      end
    catch
      :exit, {:noproc, {:gen_server, :call, [name | _]}} ->
        {:error, o, "No running process for device #{inspect(name)}"}

      :exit, _ ->
        {:error, o, "Device don't implement this functionality"}
    end
  end

  defp module(device) do
    device_ = DB.Repo.preload(device, :type)
    String.to_existing_atom("Elixir." <> device_.type.module)
  end
end
