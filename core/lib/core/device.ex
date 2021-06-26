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

  @callback need_process?() :: boolean()

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
    @callback set_fill(%DB.Device{}, [%DB.Port{}]) :: any
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
    alias DB.{Device, Port}
    @type result :: {:ok, %Port{}} | {:error, String.t()}

    @callback set_state(%Port{}) :: result
    @callback set_brightness(%Port{}) :: result
    @callback set_white_brightness(%Port{}) :: result
    @callback set_color(%Port{}) :: result
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

  alias DB.Stats.DeviceJournal, as: DJ
  alias DB.{Device, Port, Repo}
  alias Core.Device.Static.Response

  use Witchcraft
  #  use Witchcraft.Semigroup

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

  def do_(function, [%Device{} = d | t_args] = args) do
    execute_function(fn -> apply(module(d), function, args) end)
    |> DJ.log_use(function, t_args)
  end

  def do_(function, [%Port{} | _] = ports) do
    Enum.group_by(ports, & &1.device_id)
    |> Enum.map(fn {d, p} -> do_(function, [Repo.preload(d), p]) end)
    |> fold()
  end

  def do_(function, %Device{} = d), do: do_(function, [d])

  def do_r(args, function) do
    do_(function, args)
  end

  # Privates

  @spec execute_function(function()) :: Response.t()
  defp execute_function(func) do
    try do
      func.()
    catch
      :exit, {:noproc, {:gen_server, :call, [name | _]}} ->
        Response.error({:error, "No running process for device #{inspect(name)}"})

      :exit, _ ->
        Response.error({:error, "Device don't implement this functionality"})
    end
  end

  defp module(device) do
    ("Elixir.Core.Device." <> device.type)
    |> String.to_existing_atom()
  end
end
