defmodule Core.Device.SonoffBasic do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{Port, Device}
  alias Core.Device.SonoffBasic, as: Sonoff
  alias Core.Actions.ReadOutputs
  alias Core.Device.Static.Response
  import Core.Device.Client.Http

  @impl Core.Device
  def start_link(_host, _port, _opts, _keywords, _timeout \\ 5000, _length \\ 11) do
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

  @spec set_outputs(%Device{}, list(%Port{})) :: res()
  def set_outputs(%{ip: ip, port: port} = d, [%Port{number: num, state: s}] = ports) do
    state_ = if s, do: 1, else: 0
    url_ = url(ip, port, num) <> "?cmnd=Power#{num}%20#{state_}"

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> Response.wrap(d, ports)
  end

  def read_outputs(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  def read_inputs(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  def heartbeat(d) do
    {:error, "Not implemented"}
    |> Response.wrap(d)
  end

  #  Mqtt

  def handle_mqtt_result(name, payload, _) do
    with %Device{} = d <- Device.get_by_name(name) do
      %{"POWER" => state} = Poison.decode!(payload)

      if state == "ON" do
        ReadOutputs.handle_outputs(d, [0], %{last_outputs: []})
      else
        ReadOutputs.handle_outputs(d, [], %{last_outputs: [0]})
      end

      :ok
    end
  end

  # Privates

  @spec decode_body(String.t()) :: any()
  defp decode_body(body) do
    mp = Poison.decode!(body)
    for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
    #    struct(Rf, mp_atoms)
  end
end
