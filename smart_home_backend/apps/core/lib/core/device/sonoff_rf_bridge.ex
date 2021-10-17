defmodule Core.Device.SonoffRfBridge do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO

  import Core.Device.Client.Http

  alias Core.Device.Static.Response
  alias Core.Device.SonoffRfBridge, as: Rf
  alias DB.Data.{Port, Device}

  @impl Core.Device
  def start_link(_host, _port) do
    false
  end

  @impl Core.Device
  def need_process?(), do: false

  defstruct [:RfKey2]

  @type res() :: {:ok, %Rf{}} | {:error, integer()} | :conn_error

  @spec url(String.t(), integer(), integer()) :: String.t()
  defp url(ip, port, _id) do
    "#{ip}:#{port}/cm"
  end

  @impl true
  def set_outputs(%Device{ip: ip, port: port} = d, [%Port{number: num, state: s}] = ports) do
    key = if s, do: num, else: num + 1
    url_ = url(ip, port, num) <> "?cmnd=RfKey" <> to_string(key)

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  @impl true
  def read_outputs(d) do
    {:error, "Not supported"}
    |> Response.wrap(d)
  end

  @impl true
  def read_inputs(d) do
    {:error, "Not supported"}
    |> Response.wrap(d)
  end

  @impl true
  def heartbeat(d) do
    {:error, "Not supported"}
    |> Response.wrap(d)
  end

  # Privates

  @spec decode_body(String.t()) :: %Rf{}
  defp decode_body(body) do
    Poison.decode!(body)
    |> to_atom_map()
  end
end
