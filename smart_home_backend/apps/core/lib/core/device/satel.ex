defmodule Core.Device.Satel do
  @moduledoc false

  alias Core.Device
  alias Core.Device.AlarmSystem
  alias Core.Device.BasicIO

  @behaviour BasicIO
  @behaviour AlarmSystem
  @behaviour Device

  @client Core.Device.Client.Soc
  alias Core.Device.Static.Response
  alias Core.Device.Satel.Protocol
  require Logger

  use Bitwise

  @impl Device
  def start_link(host, port, opts) do
    @client.start_link(host, port, [:binary], 5000, opts)
  end

  @impl Device
  def need_process?(), do: true

  ###
  #  AlarmSystem impl
  ###
  @impl AlarmSystem
  def arm(device, mode, zones) do
    cmd = <<0xA0 + mode>>
    code = Protocol.encode_code(device.password)
    body = Protocol.encode_outputs(zones, 4)
    command = cmd <> code <> body

    run_command(device, command)
    |> Response.wrap(device, zones)
  end

  @impl Core.Device.AlarmSystem
  def disarm(device, zones) do
    code = Protocol.encode_code("***REMOVED***")
    body = Protocol.encode_outputs(zones, 4)
    command = <<0x84>> <> code <> body

    run_command(device, command)
    |> Response.wrap(device, zones)
  end

  @impl Core.Device.AlarmSystem
  def clear_alarm(device, zones) do
    code = Protocol.encode_code("***REMOVED***")
    body = Protocol.encode_outputs(zones, 4)
    command = <<0x85>> <> code <> body

    run_command(device, command)
    |> Response.wrap(device, zones)
  end

  @impl Core.Device.AlarmSystem
  def monitor_changes(device, commands \\ [0x00, 0x17]) do
    with {:ok, resp} <- run_command(device, <<0x7F>>),
         [inputs_status, outputs_status] <- Protocol.decode_commands_status(resp, commands),
         {:ok, inputs} <- if(inputs_status, do: read_inputs(device), else: {:ok, []}),
         {:ok, outputs} <- if(outputs_status, do: read_outputs(device), else: {:ok, []}) do
      {:ok, [inputs: inputs, outputs: outputs]}
    end
    |> Response.wrap(device, device)
  end

  ###
  # BasicIO impl
  ###

  @impl Core.Device.BasicIO
  def set_outputs(device, ports) do
    outputs = Enum.map(ports, & &1.number)
    state = List.first(ports).state
    code = Protocol.encode_code("***REMOVED***")
    cmd = <<if(state, do: 0x88, else: 0x89)>>
    body = Protocol.encode_outputs(outputs)
    command = cmd <> code <> body

    run_command(device, command)
    |> Response.wrap(ports)
  end

  @impl Core.Device.BasicIO
  def read_inputs(device) do
    with {:ok, resp} <- run_command(device, <<0x00>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
    |> Response.wrap_with_ports(device)
  end

  @impl Core.Device.BasicIO
  def read_outputs(device) do
    with {:ok, resp} <- run_command(device, <<0x17>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
    |> Response.wrap_with_ports(device)
  end

  @impl Core.Device.BasicIO
  def heartbeat(device) do
    {:error, "Not supported!"}
    |> Response.wrap(device)
  end

  ###
  # No impl
  ###

  def read_zones_alarm(device) do
    with {:ok, resp} <- run_command(device, <<0x02>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
    |> Response.wrap(device)
  end

  def read_zones_alarm_memory(device) do
    with {:ok, resp} <- run_command(device, <<0x04>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
    |> Response.wrap(device)
  end

  def read_armed_partitions(device, mode) do
    with {:ok, resp} <- run_command(device, <<0x09 + mode>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
    |> Response.wrap(device)
  end

  def read_partitions_alarm(device) do
    with {:ok, resp} <- run_command(device, <<0x13>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
    |> Response.wrap(device)
  end

  def read_partitions_alarm_memory(device) do
    with {:ok, resp} <- run_command(device, <<0x15>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
    |> Response.wrap(device)
  end

  def read_partitions_with_violated_zones(device) do
    with {:ok, resp} <- run_command(device, <<0x25>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
    |> Response.wrap(device)
  end

  # Privates

  defp run_command(device, <<cmd_code::binary-size(1), _::binary>> = cmd) do
    with command <- Protocol.prepare_frame(cmd),
         :ok <- @client.send(device, command),
         {:ok, resp} <- @client.recv(device, 0, 2_000) do
      Logger.log(:debug, "Send request to integra: #{inspect(command, base: :hex)}")
      Protocol.check_response(resp, cmd_code)
    else
      {:error, "Timeout"} = error ->
        Supervisor.stop(Core.Device.Supervisor, :normal)
        error
    end
  end

  defp wait_for_confirmation(timeout) do
    receive do
      resp ->
        if(
          String.slice(resp, 0..8) == "\x10\x42\x75\x73\x79\x21\x0D\x0A",
          do: {:error, "Busy"},
          else: {:ok, resp}
        )
    after
      timeout ->
        {:error, "Timeout"}
    end
  end
end
