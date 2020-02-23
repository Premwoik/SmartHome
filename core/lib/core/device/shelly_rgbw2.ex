defmodule Core.Device.ShellyRGBW2 do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{DeviceJournal}
  alias Core.DeviceMonitor
  alias Core.Device.ShellyRGBW2

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  defstruct [:ison, :mode, :red, :green, :blue, :white, :gain, :effect, :power, :overpower]

  @type res() :: {:ok, %ShellyRGBW2{}} | {:error, integer()} | :conn_error

  @spec url(string(), integer(), integer()) :: string()
  defp url(ip, port, id) do
    "#{ip}:#{port}/color/#{id}"
  end

  @spec status(%DB.Device{}, integer()) :: res()
  def status(device, port) do
    url_ = url(device.ip, device.port, port.number)

    HTTPotion.get(url_)
    |> default_response_catch()
    |> log("status", device)
  end

  @spec set_brightness(map()) :: res()
  def set_brightness(%{device: d, port: p, fill: fill}) do
    url_ = url(d.ip, d.port, p.number)
    query_ = %{"gain" => fill}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch()
    |> log("set_brightness", d)
  end

  @spec set_rgb(map()) :: res()
  def set_rgb(%{device: d, port: port, fill: fill, red: r, green: g, blue: b}) do
    url_ = url(d.ip, d.port, port.number)
    query_ = %{"red" => r, "green" => g, "blue" => b, "gain" => fill}

    HTTPotion.get(url_, query: query_)
    |> default_response_catch()
    |> log("set_rgb", d)
  end

  @spec default_response_catch(%HTTPotion.Response{}) :: res()
  defp default_response_catch(resp) do
    case resp do
      %HTTPotion.Response{status_code: 200, body: body} ->
        body_ = decode_body(body)
        {:ok, body_}

      %HTTPotion.Response{status_code: status_code} ->
        {:error, status_code}

      %HTTPotion.ErrorResponse{} ->
        :conn_error
    end
  end

  @spec decode_body(string()) :: %ShellyRGBW2{}
  defp decode_body(body) do
    mp = Poison.decode!(body)
    mp_atoms = for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
    struct(ShellyRGBW2, mp_atoms)
  end

  @spec log(res :: res(), func :: string, device :: %DB.Device{}) :: res()
  defp log(res, func, device) do
    {info, type} = case res do
      {:ok, body} -> {"#{inspect(body)}", DeviceJournal.Type.normal()}
      {:error, code} -> {"#{inspect(code)}", DeviceJournal.Type.error()}
      :conn_error -> {"Nie udało się połączyć z urządzeniem!", DeviceJournal.Type.timeout()}
    end
    DeviceJournal.log(
      device.id,
      "set_outputs",
      info = info,
      type = type
    )
    res
  end


end
