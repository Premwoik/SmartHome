defmodule Core.Device do
  @moduledoc false

  @callback start_link(
              ip :: charlist(),
              port :: integer(),
              opts :: any()
            ) :: GenServer.on_start()

  @callback need_process?() :: boolean()

  defmodule BasicIO do
    alias DB.Data.{Device, Port}
    alias Core.Device.Static.Response

    @doc """
      Sets the device's outputs.

      Returns :ok if instruction was executed correctly.
    """
    @callback set_outputs(Device.t(), [Port.t()]) :: Response.t()

    @doc """
      Reads the device's outputs.

      Returns list of the actually active outputs.
    """
    @callback read_outputs(Device.t()) :: Response.t()

    @doc """
      Reads the device's inputs.

      Returns list of the actually active inputs.
    """
    @callback read_inputs(Device.t()) :: Response.t()

    @doc """
      Sends heartbeat message to the device.

      Returns :ok if receive response from device.
    """
    @callback heartbeat(Device.t()) :: Response.t()
  end

  defmodule PwmOutputs do
    @doc """
      Sets pwm outputs
    """
    alias DB.Data.{Device, Port}
    alias Core.Device.Static.Response

    @callback set_fill(Device.t(), [Port.t()]) :: Response.t()
  end

  defmodule AlarmSystem do
    alias DB.Data.Device
    alias Core.Device.Static.Response

    @type device :: Device.t()
    @type t(res) :: {:ok, res} | {:error, String.t()}
    @type mode :: 0..3
    @type zones :: [integer]
    @type commands :: [integer]

    @callback arm(device(), mode(), zones()) :: Response.t()
    @callback disarm(device(), zones()) :: Response.t()
    @callback clear_alarm(device, zones()) :: Response.t()
    @callback monitor_changes(device(), commands()) :: Response.t()
  end

  defmodule RgbW do
    alias DB.Data.{Device, Port}
    alias Core.Device.Static.Response

    @callback set_brightness(Device.t(), [Port.t()]) :: Response.t()
    @callback set_white_brightness(Device.t(), [Port.t()]) :: Response.t()
    @callback set_color(Device.t(), [Port.t()]) :: Response.t()
  end

  defmodule Thermometer do
    alias DB.Data.{Device}
    alias Core.Device.Static.Response

    @callback read_temperatures(device :: Device.t()) :: Response.t()
  end

  defmodule EnergyMeter do
    alias DB.Data.{Device}
    alias Core.Device.Static.Response

    @callback read_counted_values(device :: Device.t()) :: Response.t()
  end

  defmodule DefaultDev do
    alias DB.Data.{Device}
    alias Core.Device.Static.Response

    @callback set_time_dimmer(device :: Device.t(), list({integer, integer})) :: Response.t()
  end

  alias DB.Data.{Device, Port}
  alias Core.Device.Static.Response

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
  @spec do_(function :: atom, args :: args_t) :: Response.t()

  def do_(function, []) do
    Response.ok([])
  end

  def do_(function, [%Device{} = d | t_args] = args) do
    {duration, res} =
      Benchmark.measure_r(fn -> execute_function(fn -> apply(module(d), function, args) end) end)

    :ok =
      :telemetry.execute(
        [:core, :device, :do],
        %{duration: duration, args: t_args, result: res},
        %{function: function, device: d}
      )

    res
  end

  def do_(function, [%Port{} | _] = ports) do
    Enum.group_by(ports, & &1.device)
    |> Enum.map(fn {d, ps} -> do_(function, [d, ps]) end)
    |> Response.fold()
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
    ("Elixir.Core.Device." <> Atom.to_string(device.type))
    |> String.to_existing_atom()
  end
end
