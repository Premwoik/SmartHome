defmodule Core.Device.SonoffBasic do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO

  import Core.Device.Client.Http

  alias Core.Broadcast, as: Channel
  alias Core.Device.SonoffBasic, as: Sonoff
  alias Core.Device.Static.Response
  alias DB.Data.{Port, Device}
  alias DB.Proc.PortListProc

  @impl Core.Device
  def start_link(_host, _port, _opts) do
    false
  end

  @impl Core.Device
  def need_process?(), do: false

  defstruct [:RfKey2]

  @type res() :: {:ok, %Sonoff{}} | {:error, integer()} | :conn_error

  @spec url(String.t(), integer(), integer()) :: String.t()
  defp url(ip, port, _id) do
    "#{ip}:#{port}/cm"
  end

  @impl true
  def set_outputs(%{ip: ip, port: port} = d, [%Port{number: num, state: s}] = ports) do
    state_ = if s["value"], do: 1, else: 0
    url_ = url(ip, port, num) <> "?cmnd=Power#{num}%20#{state_}"

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  @impl true
  def read_outputs(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  @impl true
  def read_inputs(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  @impl true
  def heartbeat(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  #  Mqtt

  def handle_mqtt_result(name, payload, _) do
    with {:ok, device} <- Device.get_by_name(name),
         [%Port{} = port] = PortListProc.identify!(device.id, [0]) do
      %{"POWER" => state} = Poison.decode!(payload)
      state = state == "ON"

      if port.state["value"] != state do
        port = PortListProc.update_state!(port.id, %{"value" => state})
        Channel.broadcast_item_change(port.type, port)
      end
    end

    :ok
  end

  # Privates

  @spec decode_body(String.t()) :: any()
  defp decode_body(body) do
    mp = Poison.decode!(body)
    for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
    #    struct(Rf, mp_atoms)
  end
end
