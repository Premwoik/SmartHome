defmodule Core.Device.SonoffRfBridge do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{DeviceJournal, Port, Device}
  alias Core.DeviceMonitor
  alias Core.Device.SonoffRfBridge, as: Rf

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  defstruct [:"RfKey2"]

  @type res() :: {:ok, %Rf{}} | {:error, integer()} | :conn_error

  @spec url(string(), integer(), integer()) :: string()
  defp url(ip, port, id) do
    "#{ip}:#{port}/cm"
  end

  @spec set_outputs(%Device{}, list(%Port{})) :: res()
  def set_outputs(%{ip: ip, port: port} = device, [%Port{device: d, number: num, state: s} = p]) do
    key = if s, do: num, else: num + 1
    url_ = url(ip, port, num) <> "?cmnd=RfKey" <> to_string(key)

    HTTPotion.get(url_)
    |> default_response_catch()
    |> log("set_output", device, p)
  end

  @spec default_response_catch(%HTTPotion.Response{}) :: res()
  defp default_response_catch(resp) do
    case resp do
      %HTTPotion.Response{status_code: 200, body: body} ->
        body_ = decode_body(body)
        {:ok, []}

      %HTTPotion.Response{status_code: status_code} ->
        {:error, status_code}

      %HTTPotion.ErrorResponse{} ->
        :conn_error
    end
  end

  @spec decode_body(string()) :: %Rf{}
  defp decode_body(body) do
    mp = Poison.decode!(body)
    mp_atoms = for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
#    struct(Rf, mp_atoms)
  end

  @spec log(res :: res(), func :: string, device :: %DB.Device{}, port :: %DB.Port{}) :: res()
  defp log(res, func, device, port) do
    {info, type} = case res do
      {:ok, body} -> {"[#{port.number}, 1]", DeviceJournal.Type.normal()}
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
