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

  def url(ip, port, id) do
    "#{ip}:#{port}/color/#{id}"
  end

  defp decode_body(body) do
   mp = Poison.decode!(body)
   mp_atoms = for {key, val} <- mp, into: %{}, do: {String.to_atom(key), val}
   struct(ShellyRGBW2, mp_atoms)
  end

  def status(device, port) do
    url_ = url(device.ip, device.port, port.number)

    case HTTPotion.get(url_) do
      %HTTPotion.Response{status_code: 200, body: body} ->
      body_ = decode_body(body)
      {:ok, body_}

      %HTTPotion.Response{status_code: status_code} ->
        {:error, status_code}

      %HTTPotion.ErrorResponse{} ->
        :conn_error
    end
  end

  def set_brightness(%{device: d, port: p, fill: fill}) do
    url_ = url(d.ip, d.port, p.number)
    query_ = %{"gain" => fill}

    case HTTPotion.get(url_, query: query_) do
      %HTTPotion.Response{status_code: 200, body: body} ->
        body_ = decode_body(body)
        {:ok, body_}

      %HTTPotion.Response{status_code: status_code} ->
        :ok

      %HTTPotion.ErrorResponse{} ->
        :ok
    end
  end

  def set_rgb(%{device: d, port: port, fill: fill, red: r, green: g, blue: b}) do
    url_ = url(d.ip, d.port, port.number)
    query_ = %{"red" => r, "green" => g, "blue" => b, "gain" => fill}

    case HTTPotion.get(url_, query: query_) do
      %HTTPotion.Response{status_code: 200, body: body} ->
        body_ = decode_body(body)
        {:ok, body_}

      %HTTPotion.Response{status_code: status_code} ->
        :ok

      %HTTPotion.ErrorResponse{} ->
        :ok
    end
  end

  #  @impl true
  #  def set_outputs(%{ip: ip, port: port} = device, ports) do
  #    port_ = List.first(ports)
  #    state = if port_.state, do: "on", else: "off"
  #    url_ = url(ip, port, port_.number)
  #
  #    case HTTPotion.get(
  #           url_,
  #           query: %{
  #             "turn" => state
  #           }
  #         ) do
  #      %HTTPotion.Response{status_code: 200, body: body} ->
  #        %{"ison" => state_} = Poison.decode!(body)
  #
  #        if state_ == port_.state do
  #          DeviceJournal.log(
  #            device.id,
  #            "set_outputs",
  #            info = "Zmiana stanu portu #{port_.name} na #{state}."
  #          )
  #
  #          {:ok, []}
  #        else
  #          DeviceJournal.log(
  #            device.id,
  #            "set_outputs",
  #            type = DeviceJournal.Type.error(),
  #            info = "Nie udało się zmienić stan portu #{port_.name} na #{state_}."
  #          )
  #
  #          {:error, "cannot set propper state"}
  #        end
  #
  #      %HTTPotion.Response{status_code: status_code} ->
  #        DeviceJournal.log(
  #          device.id,
  #          "set_outputs",
  #          type = DeviceJournal.Type.error(),
  #          info = "Zły kod odpowiedzi - zmiana stanu portu #{port_.name} na #{state}.
  #          Prawdopodobnie urządzenie nie posiada takiego portu."
  #        )
  #
  #        {:error, "wrong response code"}
  #
  #      %HTTPotion.ErrorResponse{} ->
  #        DeviceMonitor.connection_error(device, "Cannot connect to device")
  #        {:error, "cannot connect to device"}
  #    end
  #  end
  #
  #  @impl true
  #  def read_outputs(%{ip: ip, port: port, id: id} = device) do
  #    # TODO change hardcoded port (pin) number
  #    url_ = url(ip, port, 0)
  #
  #    case HTTPotion.get(url_) do
  #      %HTTPotion.Response{status_code: 200, body: body} ->
  #        %{"ison" => state_} = Poison.decode!(body)
  #        DeviceJournal.log(id, "read_outputs", info = "Odczytane stany #{state_}.")
  #        {:ok, [{0, state_}]}
  #
  #      %HTTPotion.Response{status_code: status_code} ->
  #        DeviceJournal.log(
  #          id,
  #          "read_outputs",
  #          type = DeviceJournal.Type.error(),
  #          info = "Zły kod odpowiedzi"
  #        )
  #
  #        {:error, "wrong response code"}
  #
  #      %HTTPotion.ErrorResponse{} ->
  #        DeviceMonitor.connection_error(device, "Cannot connect to device")
  #        {:error, "cannot connect to device"}
  #    end
  #  end
end
