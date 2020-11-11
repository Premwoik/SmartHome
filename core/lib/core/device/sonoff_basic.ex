defmodule Core.Device.SonoffBasic do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{DeviceJournal, Port, Device}
  alias Core.DeviceMonitor
  alias Core.Device.SonoffBasic, as: Sonoff
  alias Core.Tasks.ReadOutputs

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  defstruct [:"RfKey2"]

  @type res() :: {:ok, %Sonoff{}} | {:error, integer()} | :conn_error

  @spec url(string(), integer(), integer()) :: string()
  defp url(ip, port, id) do
    "#{ip}:#{port}/cm"
  end

  @spec set_outputs(%Device{}, list(%Port{})) :: res()
  def set_outputs(%{ip: ip, port: port} = device, [%Port{device: d, number: num, state: s} = p]) do
    url_ = url(ip, port, num) <> "?cmnd=Power#{num} #{s}"

    HTTPotion.get(url_)
    |> default_response_catch()
  end

  def handle_mqtt_result(name, payload, _) do
    with %Device{} = d <-  Device.get_by_name(name) do
      %{"POWER" => state} = Poison.decode!(payload)
      if state == "ON" do
        ReadOutputs.handle_outputs(d, [0], %{last_outputs: []})
      else
        ReadOutputs.handle_outputs(d, [], %{last_outputs: [0]})
      end
      :ok
    end
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

  @spec decode_body(string()) :: any()
  defp decode_body(body) do
    mp = Poison.decode!(body)
    mp_atoms = for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
#    struct(Rf, mp_atoms)
  end

end
