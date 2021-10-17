defmodule Core.Device.OriginalArduino do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO
  @behaviour Core.Device.PwmOutputs

  alias Core.Device.BasicIO
  alias Core.Device.PwmOutputs
  import Core.Device.Client.Http
  alias Core.Device.Static.Response

  alias DB.{Device}

  @impl Core.Device
  def start_link(_, _, _, _, _, _) do
    false
  end

  @impl Core.Device
  def need_process?(), do: false

  @impl BasicIO
  def set_outputs(%{ip: ip, port: port} = device, ports) do
    pairs = Enum.map(ports, fn p -> %{pin: p.number, state: p.state} end) |> Poison.encode!()
    path = "#{device.ip}:#{device.port}/write"  
    HTTPotion.post(path, headers: ["Content-Type": "application/json"], body: pairs)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(device, ports)
  end

  @impl BasicIO
  def read_outputs(%{ip: ip, port: port} = d) do
    {:error, "Not implemented"} |> Response.wrap(d)
  end

  @impl BasicIO
  def read_inputs(d), do: {:error, "Not implemented"} |> Response.wrap(d)

  @impl BasicIO
  def heartbeat(d), do: {:error, "Not implemented"} |> Response.wrap(d)

  @impl PwmOutputs 
  def set_fill(device, ports) do
    pairs = Enum.map(ports, fn p -> %{pin: p.number, fill: p.more.fill} end) |> Poison.encode!()
    path = "#{device.ip}:#{device.port}/fill"  
    HTTPotion.post(path, headers: ["Content-Type": "application/json"], body: pairs)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(device, ports)
  end

  # Mqtt

  def handle_mqtt_result(name, state, _) do
    :ok
  end

  #Privates
  @spec decode_body(String.t()) :: %{}
  defp decode_body(body) do
    Poison.decode!(body) 
  end

end