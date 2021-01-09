defmodule Core.Device.Shelly do
  @moduledoc false

  @behaviour Core.Device
  @behaviour Core.Device.BasicIO
  # , only: [default_response_catch: 2]
  import Core.Device.Client.Http

  alias DB.{Device}
  alias Core.Tasks.ReadOutputs

  @impl Core.Device
  def start_link(_, _, _, _, _, _) do
    false
  end

  defstruct [:ison, :mode, :red, :green, :blue, :white, :gain, :effect, :power, :overpower]

  def url(ip, port, id) do
    "#{ip}:#{port}/relay/#{id}"
  end

  @impl true
  def set_outputs(%{ip: ip, port: port}, ports) do
    port_ = List.first(ports)
    state = if port_.state, do: "on", else: "off"
    url_ = url(ip, port, port_.number)

    HTTPotion.get(url_, query: %{"turn" => state})
    |> default_response_catch(&decode_body/1)
    |> skip_response()
  end

  @impl true
  def read_outputs(%{ip: ip, port: port}) do
    # TODO change hardcoded port (pin) number
    url_ = url(ip, port, 0)

    HTTPotion.get(url_)
    |> default_response_catch(&decode_body/1)
    |> format_read_outputs()
  end

  @impl true
  def read_inputs(_), do: {:error, "Not implemented"}

  @impl true
  def heartbeat(_), do: {:error, "Not implemented"}

  # Mqtt

  def handle_mqtt_result(name, state, _) do
    with %Device{} = d <- Device.get_by_name(name) do
      if state == "on" do
        ReadOutputs.handle_outputs(d, [0], %{last_outputs: []})
      else
        ReadOutputs.handle_outputs(d, [], %{last_outputs: [0]})
      end

      :ok
    end
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
