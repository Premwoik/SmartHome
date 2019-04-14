defmodule Core.Device.Shelly do
  @moduledoc false

  @behaviour Core.Device


  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    false
  end

  def url(ip, port, id) do
    "#{ip}:#{port}/relay/#{id}"
  end

  @impl true
  def set_outputs(%{ip: ip, port: port} = device, ports) do
    port_ = List.first(ports)
    IO.inspect port_
    state_ = if port_.state do "on" else "off" end
    url_ = url(ip, port, port_.number)
    resp = HTTPotion.get(url_, query: %{"turn" => state_})
    case resp.status_code do
      200 ->
        %{"ison" => state_} = Poison.decode! resp.body
        if state_ == port_.state do
          {:ok, []}
        else
          {:error, "cannot set propper state"}
        end
      _else -> {:error, "wrong response code"}
    end
  end
  
  @impl true
  def read_outputs(%{ip: ip, port: port}) do
    #TODO change hardcoded port (pin) number
    url_ = url(ip, port, 0)
    resp = HTTPotion.get(url_)
    case resp.status_code do
      200 -> 
        %{"ison" => state_} = Poison.decode! resp.body
        {:ok, [{0, state_}]}
      _else -> {:error, "wrong response code"}
    end   
  end
  
end
