defmodule Core.Device.Shelly do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO

  import Core.Device.Client.Http

  alias Core.Broadcast, as: Channel
  alias Core.Device.BasicIO
  alias Core.Device.Static.Response
  alias DB.Data.Device
  alias DB.Data.Port
  alias DB.Proc.PortListProc

  @impl Core.Device
  def start_link(_, _, _) do
    false
  end

  @impl Core.Device
  def need_process?(), do: false

  defstruct [:ison, :mode, :red, :green, :blue, :white, :gain, :effect, :power, :overpower]

  def url(ip, port, id) do
    "#{ip}:#{port}/relay/#{id}"
  end

  @impl BasicIO
  def set_outputs(%Device{ip: ip, port: port} = device, ports) do
    port_ = List.first(ports)
    state = if port_.state["value"], do: "on", else: "off"
    url_ = url(ip, port, port_.number)

    HTTPotion.get(url_, query: %{"turn" => state})
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(device, ports)
  end

  @impl BasicIO
  def read_outputs(%Device{ip: ip, port: port} = d) do
    # TODO change hardcoded port (pin) number
    url_ = url(ip, port, 0)

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> format_read_outputs()
    |> Response.wrap_same(d)
  end

  @impl BasicIO
  def read_inputs(d), do: {:error, "Not implemented"} |> Response.wrap(d)

  @impl BasicIO
  def heartbeat(d), do: {:error, "Not implemented"} |> Response.wrap(d)

  # Mqtt

  def handle_mqtt_result(name, payload, _state) do
    with {:ok, device} <- Device.get_by_name(name),
         [%Port{} = port] = PortListProc.identify!(device.id, [0]) do
      state = payload == "on"

      if port.state["value"] != state do
        port = PortListProc.update_state!(port.id, %{"value" => state})
        Channel.broadcast_item_change(port.type, port)
      end
    end

    :ok
  end

  #  Privates

  @spec decode_body(String.t()) :: %{}
  defp decode_body(body) do
    Poison.decode!(body)
    |> to_atom_map()
  end

  defp format_read_outputs({:ok, %{ison: status}}),
    do: if(status, do: [0], else: [])

  defp format_read_outputs(err),
    do: err
end
