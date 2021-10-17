defmodule Core.Device.ShellyRGBW2 do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.RgbW

  require Logger
  import Core.Device.Client.Http

  alias Core.Device.Static.Response
  alias Core.Device.ShellyRGBW2
  alias DB.Data.Port

  @impl Core.Device
  def start_link(_host, _port) do
    false
  end

  @impl Core.Device
  def need_process?(), do: false

  defstruct [:ison, :mode, :red, :green, :blue, :white, :gain, :effect, :power, :overpower]

  @type res() :: {:ok, %ShellyRGBW2{}} | {:error, integer()} | :conn_error

  @spec url(String.t(), integer(), integer()) :: String.t()
  defp url(ip, port, id) do
    "#{ip}:#{port}/color/#{id}"
  end

  # FIXME change to set_outputs from BasicIO
  def set_state(%{ip: ip, port: port} = d, [%Port{state: s, number: num}] = ports) do
    url_ = url(ip, port, num)
    s = if(s, do: "on", else: "off")
    query = %{"turn" => s}

    HTTPotion.get(url_, query: query)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  @impl true
  def set_brightness(
        %{ip: ip, port: port} = d,
        [%Port{number: num, state: %{"brightness" => fill}}] = ports
      ) do
    url_ = url(ip, port, num)

    query_ = %{"gain" => fill}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  @impl true
  def set_white_brightness(
        %{ip: ip, port: port} = d,
        [%Port{number: num, state: %{"white" => w}}] = ports
      ) do
    url_ = url(ip, port, num)
    query_ = %{"white" => w}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  @impl true
  def set_color(
        %{ip: ip, port: port} = d,
        [%Port{number: num, state: %{"color" => %{"red" => r, "green" => g, "blue" => b}}}] =
          ports
      ) do
    url_ = url(ip, port, num)
    query_ = %{"red" => r, "green" => g, "blue" => b}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  # Privates

  @spec decode_body(String.t()) :: %ShellyRGBW2{}
  defp decode_body(body) do
    mp_atoms =
      Poison.decode!(body)
      |> to_atom_map()

    Logger.debug("ShellyRgbW response = #{inspect(mp_atoms)}")
    struct(ShellyRGBW2, mp_atoms)
  end
end
