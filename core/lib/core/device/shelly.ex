defmodule Core.Device.Shelly do
  @moduledoc false

  @behaviour Core.Device
  alias DB.{DeviceJournal}


  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  def url(ip, port, id) do
    "#{ip}:#{port}/relay/#{id}"
  end

  @impl true
  def set_outputs(%{ip: ip, port: port} = device, ports) do
    port_ = List.first(ports)
    state = if port_.state do "on" else "off" end
    url_ = url(ip, port, port_.number)
    resp = HTTPotion.get(url_, query: %{"turn" => state})
    case resp.status_code do
      200 ->
        %{"ison" => state_} = Poison.decode! resp.body
        if state_ == port_.state do
          DeviceJournal.log(device.id, "set_outputs", info = "Zmiana stanu portu #{port_.name} na #{state}.")
          {:ok, []}
        else
          DeviceJournal.log(device.id, "set_outputs", type= DeviceJournal.Type.error,
            info = "Nie udało się zmienić stan portu #{port_.name} na #{state_}.")
          {:error, "cannot set propper state"}
        end
      _else -> 
        DeviceJournal.log(device.id, "set_outputs", type= DeviceJournal.Type.error,
          info = "Zły kod odpowiedzi - zmiana stanu portu #{port_.name} na #{state}.
          Prawdopodobnie urządzenie nie posiada takiego portu.")
        {:error, "wrong response code"}
    end
  end
  
  @impl true
  def read_outputs(%{ip: ip, port: port, id: id}) do
    #TODO change hardcoded port (pin) number
    url_ = url(ip, port, 0)
    resp = HTTPotion.get(url_)
    case resp.status_code do
      200 -> 
        %{"ison" => state_} = Poison.decode! resp.body
        DeviceJournal.log(id, "read_outputs", info = "Odczytane stany #{state_}.")
        {:ok, [{0, state_}]}
      _else -> 
        DeviceJournal.log(id, "read_outputs", type= DeviceJournal.Type.error,
          info = "Zły kod odpowiedzi")
        {:error, "wrong response code"}
    end   
  end
end
