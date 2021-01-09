defmodule Core.Device.ShellyRGBW2 do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.RgbW

  require Logger

  alias Core.Device.ShellyRGBW2
  # , only: [default_response_catch: 2]
  import Core.Device.Client.Http

  @impl Core.Device
  def start_link(_host, _port, _opts, _keywords, _timeout \\ 5000, _length \\ 11) do
    false
  end

  defstruct [:ison, :mode, :red, :green, :blue, :white, :gain, :effect, :power, :overpower]

  @type res() :: {:ok, %ShellyRGBW2{}} | {:error, integer()} | :conn_error

  @spec url(String.t(), integer(), integer()) :: String.t()
  defp url(ip, port, id) do
    "#{ip}:#{port}/color/#{id}"
  end

  #  @spec status(%DB.Device{}, integer()) :: res()
  #  def status(device, port) do
  #    url_ = url(device.ip, device.port, port.number)
  #
  #    HTTPotion.get(url_)
  #    |> default_response_catch(&decode_body/1)
  #  end

  @impl true
  def set_state(%{port: %{device: d, state: s, number: num}} = dimmer) do
    url_ = url(d.ip, d.port, num)
    s = if(s, do: "on", else: "off")
    query = %{"turn" => s}

    HTTPotion.get(url_, query: query)
    |> default_response_catch(&decode_body/1)
    |> skip_response(dimmer)
  end

  @impl true
  def set_brightness(%{port: %{device: d, number: num}, fill: fill} = dimmer) do
    IO.inspect(dimmer)
    url_ = url(d.ip, d.port, num)
    query_ = %{"gain" => fill}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> skip_response(dimmer)
  end

  @impl true
  def set_white_brightness(%{port: %{device: d, number: num}, white: w} = dimmer) do
    url_ = url(d.ip, d.port, num)
    query_ = %{"white" => w}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> skip_response(dimmer)
  end

  @impl true
  @spec set_color(map()) :: res()
  def set_color(%{port: %{device: d, number: num}, red: r, green: g, blue: b} = dimmer) do
    url_ = url(d.ip, d.port, num)
    query_ = %{"red" => r, "green" => g, "blue" => b}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch(&decode_body/1)
    |> skip_response(dimmer)
  end

  @spec decode_body(String.t()) :: %ShellyRGBW2{}
  defp decode_body(body) do
    mp_atoms =
      Poison.decode!(body)
      |> to_atom_map()

    Logger.debug("ShellyRgbW response = #{inspect(mp_atoms)}")
    struct(ShellyRGBW2, mp_atoms)
  end
end
