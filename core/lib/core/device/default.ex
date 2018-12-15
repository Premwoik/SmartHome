defmodule Core.Device.Default do
  @moduledoc false
  require Logger
  use Bitwise

  defmodule Code do
    @moduledoc false

    def setOutputs, do: 100
    def readTemp, do: 110
    def readInputs, do: 112
    def readWattmeters, do: 111
    def setPwmOutputs, do: 105
    def heartbeat, do: 200


  end

  @behaviour Core.Device

  @client Application.get_env(:core, :two_way_client)
  @protocol Core.Device.Default.Protocol

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    IO.inspect(@client)
    @client.start_link(host, port, opts, keywords, timeout, length)
  end

  @impl true
  def read_active_inputs(device) do
    cmd = Code.readInputs()
    noreply_send(device, cmd, [])
  end


  @impl true
  def set_outputs(device, data) do
    Logger.info("#{device.name} - set_outputs - #{inspect data}")
    cmd = Code.setOutputs()
    noreply_send(device, cmd, data)
  end

  @impl true
  def set_pwm_outputs(device, data) do
    cmd = Code.setPwmOutputs()
    noreply_send(device, cmd, data)
  end

  @impl true
  def read_counted_values(device) do
    noreply_send(device, Code.readWattmeters(), [])
  end

  def heartbeat(device) do
    noreply_send(device, Code.heartbeat(), [])
  end

  @impl true
  def read_temperatures(device) do
    noreply_send(device, Code.readTemp(), [])
  end



  # Privates

  defp noreply_send(device, cmd, args) do
    msg = @protocol.encode(0, cmd, args)
    case @client.send_with_resp device, msg do
      :ok -> wait_for_confirmation()
      error -> error
    end
  end

  defp wait_for_confirmation(timeout \\ 1_000) do
    receive do
      msg ->
        case @protocol.decode msg do
          {:ok, [cmd, status | args]} ->
            case status == 200 do
              true -> {:ok, args}
              false -> {:error, "wrong response code"}
            end
          error -> error
        end
    after
      timeout -> {:error, "confirmation timeout"}
    end
  end
end
