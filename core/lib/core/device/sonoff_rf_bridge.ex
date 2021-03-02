defmodule Core.Device.SonoffRfBridge do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{Port, Device}
  alias Core.Device.Static.Response
  alias Core.Device.SonoffRfBridge, as: Rf
  import Core.Device.Client.Http

  @impl Core.Device
  def start_link(_host, _port, _opts, _keywords, _timeout \\ 5000, _length \\ 11) do
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

  @spec set_outputs(%Device{}, list(%Port{})) :: res()
  def set_outputs(%{ip: ip, port: port} = d, [%Port{number: num, state: s}] = ports) do
    key = if s, do: num, else: num + 1
    url_ = url(ip, port, num) <> "?cmnd=RfKey" <> to_string(key)

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  # Privates

  @spec decode_body(String.t()) :: %Rf{}
  defp decode_body(body) do
    Poison.decode!(body)
    |> to_atom_map()
  end
end
