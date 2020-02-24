defmodule Core.Device.Default do
  @moduledoc false
  require Logger
  use Bitwise

  alias DB.DeviceJournal

  defmodule Code do
    @moduledoc false

    def setOutputs, do: 100
    def readTemp, do: 110
    def readInputs, do: 112
    def readWattmeters, do: 111
    def setPwmOutputs, do: 105
    def heartbeat, do: 200
  end

  @behaviour Core.Controllers.PortController
  @behaviour Core.Controllers.ThermometerController
  @behaviour Core.Controllers.WattmeterController

  @client Application.fetch_env!(:core, :two_way_client)
  @protocol Core.Device.Default.Protocol

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    @client.start_link(host, port, opts, keywords, timeout, length)
  end

  @impl true
  def read_active_inputs(device) do
    DeviceJournal.log(device.id, "read_active_inputs", info = "Odczytanie aktywnych wejść urządzenia")
    cmd = Code.readInputs()
    noreply_send(device, cmd, [])
  end

  @impl true
  def set_outputs(device, ports) do
    cmd = Code.setOutputs()
    data = ports_to_num(ports)
    DeviceJournal.log(device.id, "set_outputs", info="Ustawianie portów #{inspect(data)}")
    noreply_send(device, cmd, data)
  end

  @impl true
  def set_pwm_outputs(device, ports) do
    DeviceJournal.log(device.id, "set_pwm_outputs", info="")
    cmd = Code.setPwmOutputs()
    data = Enum.flat_map(ports, fn p -> [p.number, p.pwm_fill] end)
    noreply_send(device, cmd, data)
  end

  @impl true
  def read_watts(device) do
    DeviceJournal.log(device.id, "read_watts", info ="")
    noreply_send(device, Code.readWattmeters(), [])
  end

  def heartbeat(device) do
    DeviceJournal.log(device.id, "heartbeat", info = "")
    noreply_send(device, Code.heartbeat(), [])
  end

  @impl true
  def read_temperatures(device) do
    DeviceJournal.log(device.id, "read_temperatures", info= "")
    case noreply_send(device, Code.readTemp(), []) do
      {:ok, val} ->
        {addr, [h, l]} = Enum.split(val, 8)
        raw = h <<< 8 ||| l
        {:ok, {to_string(addr), raw}}

      err_ ->
        err_
    end
  end

  # Privates
  defp noreply_send(device, cmd, args, try_max \\ 5)

  defp noreply_send(_, _, _, 0),
    do: {:error, "All allowed attempts failed. Can't get response!"}

  defp noreply_send(device, cmd, args, max_try) do
    msg = @protocol.encode(0, cmd, args)

    case @client.send_with_resp(device, msg) do
      :ok ->
        case wait_for_confirmation() do
          :timeout -> noreply_send(device, cmd, args)
          response -> response
        end

      error ->
        error
    end
  end

  defp wait_for_confirmation(timeout \\ 2_000) do
    receive do
      msg ->
        case @protocol.decode(msg) do
          {:ok, [cmd, status | args]} ->
            case status == 200 do
              true -> {:ok, args}
              false -> {:error, "wrong response code"}
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

  defp state_to_num(false), do: 0
  defp state_to_num(_), do: 1
end
