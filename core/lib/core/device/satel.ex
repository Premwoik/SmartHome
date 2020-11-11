defmodule Core.Device.Satel do
  @moduledoc false

  @behaviour Core.Device.BasicIO
  @behaviour Core.Device.AlarmSystem

  @client Application.fetch_env!(:core, :two_way_client)
  alias Core.Device.Satel.Protocol
  require Logger

  use Bitwise

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    @client.start_link(host, port, [:binary], keywords, timeout, length)
  end

  ###
  #  AlarmSystem impl
  ###

  def arm(device, mode, zones) do
    cmd = <<0xA0 + mode>>
    code = Protocol.encode_code(device.password)
    body = Protocol.encode_outputs(zones, 4)
    command = cmd <> code <> body
    |> IO.inspect()
    with {:ok, resp} <- run_command(device, command) do
      {:ok, zones}
    end
  end

  def disarm(device, zones) do
    code = Protocol.encode_code("***REMOVED***")
    body = Protocol.encode_outputs(zones, 4)
    command = <<0x84>> <> code <> body
    with {:ok, resp} <- run_command(device, command) do
      {:ok, zones}
    end
  end

  def clear_alarm(device, zones) do
    code = Protocol.encode_code("***REMOVED***")
    body = Protocol.encode_outputs(zones, 4)
    command = <<0x85>> <> code <> body
    with {:ok, resp} <- run_command(device, command) do
      {:ok, zones}
    end
  end


  def monitor_changes(device, commands \\ [0x00, 0x17]) do
    with {:ok, resp} <- run_command(device, <<0x7F>>),
         [inputs_status, outputs_status] <- Protocol.decode_commands_status(resp, commands),
         {:ok, inputs} <- if(inputs_status, do: read_inputs(device), else: {:ok, []}),
         {:ok, outputs} <- if(outputs_status, do: read_outputs(device), else: {:ok, []}) do
      {:ok, [inputs: inputs, outputs: outputs]}
    end
  end

  ###
  # BasicIO impl
  ###

  @impl true
  def set_outputs(device, outputs, state) do
    code = Protocol.encode_code("***REMOVED***")
    cmd = <<if(state, do: 0x88, else: 0x89)>>
    body = Protocol.encode_outputs(outputs)
    command = cmd <> code <> body
    with {:ok, resp} <- run_command(device, command) do
      {:ok, {state, outputs}}
    end
  end

  @impl true
  def read_inputs(device) do
    with {:ok, resp} <- run_command(device, <<0x00>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
  end

  @impl true
  def read_outputs(device) do
    with {:ok, resp} <- run_command(device, <<0x17>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
  end

  @impl true
  def heartbeat(device) do
    :ok
  end

  ###
  # No impl
  ###

  def read_zones_alarm(device) do
    with {:ok, resp} <- run_command(device, <<0x02>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
  end

  def read_zones_alarm_memory(device) do
    with {:ok, resp} <- run_command(device, <<0x04>>) do
      {:ok, Protocol.decode_outputs(resp, 8)}
    end
  end

  def read_armed_partitions(device, mode) do
    with {:ok, resp} <- run_command(device, <<0x09 + mode>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
  end
  def read_partitions_alarm(device) do
    with {:ok, resp} <- run_command(device, <<0x13>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
  end
  def read_partitions_alarm_memory(device) do
    with {:ok, resp} <- run_command(device, <<0x15>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
  end
  def read_partitions_with_violated_zones(device) do
    with {:ok, resp} <- run_command(device, <<0x25>>) do
      {:ok, Protocol.decode_outputs(resp, 4)}
    end
  end

  # Privates

  defp run_command(device, <<cmd_code :: binary - size(1), _ :: binary>> = cmd) do
    with command <- Protocol.prepare_frame(cmd),
         :ok <- @client.send_with_resp(device, command),
         {:ok, resp} <- wait_for_confirmation(5_000) do
      Logger.log(:debug, "Send request to integra: #{inspect command, base: :hex}")
      Protocol.check_response(resp, cmd_code)
    else
      err -> err
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
