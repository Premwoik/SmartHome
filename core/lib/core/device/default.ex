defmodule Core.Device.Default do
  @moduledoc false
  require Logger
  use Bitwise
  alias Core.Device.Static.Response

  defmodule Code do
    @moduledoc false

    def setOutputs, do: 100
    def setTimeDimmer, do: 102
    def readTemp, do: 110
    def readInputs, do: 112
    def readWattMeters, do: 111
    def setPwmOutputs, do: 105
    def heartbeat, do: 200
  end

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO
  @behaviour Core.Device.PwmOutputs
  @behaviour Core.Device.Thermometer
  @behaviour Core.Device.EnergyMeter
  @behaviour Core.Device.DefaultDev

  @client Application.fetch_env!(:core, :two_way_client)
  @protocol Core.Device.Default.Protocol

  @impl true
  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    @client.start_link(host, port, opts, keywords, timeout, length)
  end

  @impl true
  def need_process?(), do: true

  def read_active_inputs(device), do: read_inputs(device)

  @impl true
  def read_inputs(device) do
    cmd = Code.readInputs()

    noreply_send(device, cmd, [])
    |> Response.wrap_with_ports(device)
  end

  @impl true
  def read_outputs(device) do
    {:error, "Not implemented yet"}
    |> Response.wrap_with_ports(device)
  end

  @impl true
  def set_outputs(device, ports) do
    cmd = Code.setOutputs()
    data = ports_to_num(ports)

    noreply_send(device, cmd, data)
    |> Response.wrap(device, ports)
  end

  @impl true
  def set_time_dimmer(device, ports) do
    IO.puts("Default")
    IO.inspect(ports)
    cmd = Code.setTimeDimmer()
    data = time_ports_to_num(ports)

    noreply_send(device, cmd, data)
    |> Response.wrap(device, ports)
  end

  @impl true
  def set_pwm_outputs(device, ports) do
    cmd = Code.setPwmOutputs()
    data = Enum.flat_map(ports, fn p -> [p.number, p.pwm_fill] end)

    noreply_send(device, cmd, data)
    |> Response.wrap(device, ports)
  end

  @impl true
  def heartbeat(device) do
    noreply_send(device, Code.heartbeat(), [])
    |> Response.wrap(device)
  end

  @impl true
  def read_counted_values(device) do
    noreply_send(device, Code.readWattMeters(), [])
    |> Response.wrap(device)
  end

  @impl true
  def read_temperatures(device) do
    with {:ok, val} <- noreply_send(device, Code.readTemp(), []) do
      {addr, [h, l]} = Enum.split(val, 8)
      raw = h <<< 8 ||| l
      {:ok, [{to_string(addr), raw}]}
    end
    |> Response.wrap_same(device)
  end

  # Privates

  defp noreply_send(device, cmd, args, try_max \\ 5)

  defp noreply_send(_, _, _, 0),
    do: {:error, "All allowed attempts failed. Can't get response!"}

  defp noreply_send(device, cmd, args, _max_try) do
    msg = @protocol.encode(0, cmd, args)

    case @client.send_with_resp(device, msg) do
      :ok ->
        case wait_for_confirmation() do
          :timeout ->
            noreply_send(device, cmd, args)

          response ->
            response
        end

      error ->
        error
    end
  end

  defp wait_for_confirmation(timeout \\ 2_000) do
    receive do
      msg ->
        case @protocol.decode(msg) do
          {:ok, [_cmd, status | args]} ->
            case status == 200 do
              true -> {:ok, args}
              false -> {:error, "Wrong response code"}
            end

          error ->
            error
        end
    after
      timeout -> :timeout
    end
  end

  defp ports_to_num(ports) do
    ports
    |> Enum.flat_map(fn p ->
      if p.inverted_logic,
        do: [p.number, state_to_num(not p.state)],
        else: [p.number, state_to_num(p.state)]
    end)
  end

  defp time_ports_to_num(ports) do
    ports
    |> Enum.flat_map(fn p ->
      [p.number, 0, round(p.timeout / 100)]
    end)
  end

  defp state_to_num(false), do: 0
  defp state_to_num(_), do: 1
end
