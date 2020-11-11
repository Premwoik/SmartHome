defmodule Core.Device.Shelly do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{DeviceJournal, Device}
  alias Core.DeviceMonitor
  alias Core.Tasks.ReadOutputs

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  def url(ip, port, id) do
    "#{ip}:#{port}/relay/#{id}"
  end

  @impl true
  def set_outputs(%{ip: ip, port: port} = device, ports) do
    port_ = List.first(ports)
    state = if port_.state, do: "on", else: "off"
    url_ = url(ip, port, port_.number)

    case HTTPotion.get(url_, query: %{"turn" => state}) do
      %HTTPotion.Response{status_code: 200, body: body} ->
        %{"ison" => state_} = Poison.decode!(body)

        if state_ == port_.state do
          DeviceJournal.log(
            device.id,
            "set_outputs",
            info = "Zmiana stanu portu #{port_.name} na #{state}."
          )

          {:ok, []}
        else
          DeviceJournal.log(
            device.id,
            "set_outputs",
            type = DeviceJournal.Type.error(),
            info = "Nie udało się zmienić stan portu #{port_.name} na #{state_}."
          )

          {:error, "cannot set propper state"}
        end

      %HTTPotion.Response{status_code: status_code} ->
        DeviceJournal.log(
          device.id,
          "set_outputs",
          type = DeviceJournal.Type.error(),
          info = "Zły kod odpowiedzi - zmiana stanu portu #{port_.name} na #{state}.
          Prawdopodobnie urządzenie nie posiada takiego portu."
        )

        {:error, "wrong response code"}

      %HTTPotion.ErrorResponse{} ->
        DeviceMonitor.connection_error(device, "Cannot connect to device")
        {:error, "cannot connect to device"}
    end
  end

  @impl true
  def read_outputs(%{ip: ip, port: port, id: id} = device) do
    # TODO change hardcoded port (pin) number
    url_ = url(ip, port, 0)

    case HTTPotion.get(url_) do
      %HTTPotion.Response{status_code: 200, body: body} ->
        %{"ison" => state_} = Poison.decode!(body)
        DeviceJournal.log(id, "read_outputs", info = "Odczytane stany #{state_}.")
        {:ok, [{0, state_}]}

      %HTTPotion.Response{status_code: status_code} ->
        DeviceJournal.log(
          id,
          "read_outputs",
          type = DeviceJournal.Type.error(),
          info = "Zły kod odpowiedzi"
        )

        {:error, "wrong response code"}

      %HTTPotion.ErrorResponse{} ->
        DeviceMonitor.connection_error(device, "Cannot connect to device")
        {:error, "cannot connect to device"}
    end
  end

  def handle_mqtt_result(name, state, _) do
    with %Device{} = d <-  Device.get_by_name(name) do
      if state == "on" do
        ReadOutputs.handle_outputs(d, [0], %{last_outputs: []})
      else
        ReadOutputs.handle_outputs(d, [], %{last_outputs: [0]})
      end
      :ok
    end
  end
end
