defmodule Core.Device.Client.TwoWay do
  @moduledoc false

  use Connection
  require Logger

  @behaviour Core.Device.Client

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    Logger.info("Initializing Device.Client #{host}, #{port}")
    Connection.start_link(__MODULE__, {host, port, opts, timeout, length}, keywords)
  end

  ## Public

  @impl true
  def send_with_resp(device, msg) do
    device_name(device)
    |> Connection.call({:confirm_send, msg})
  end

  @impl true
  def send_msg(device, msg) do
    device_name(device)
    |> Connection.call({:send, msg})
  end

  defp device_name(device) do
    String.to_atom(device.name)
  end

  ## Callbacks

  @impl true
  def init({host, port, opts, timeout, length}) do
    s = %{
      host: host,
      port: port,
      opts: opts,
      timeout: timeout,
      length: length,
      sock: nil,
      observers: :queue.new()
    }

    {:connect, :init, s}
  end

  @impl true
  def connect(
        _,
        %{
          sock: nil,
          host: host,
          port: port,
          opts: opts,
          timeout: timeout
        } = s
      ) do
    case :gen_tcp.connect(host, port, opts, timeout) do
      {:ok, sock} ->
        {:ok, %{s | sock: sock}}

      {:error, _} ->
        Logger.error("Cannot connect to device #{host}:#{to_string(port)}")
        {:backoff, 1000, s}
    end
  end

  @impl true
  def disconnect(info, %{sock: sock} = s) do
    :ok = :gen_tcp.close(sock)

    case info do
      {:close, from} ->
        Connection.reply(from, :ok)

      {:error, :closed} ->
        :error_logger.format("Connection closed~n", [])

      {:error, reason} ->
        reason = :inet.format_error(reason)
        :error_logger.format("Connection error: ~s~n", [reason])
    end

    {:connect, :reconnect, %{s | sock: nil}}
  end

  @impl true
  def handle_info({:tcp, _port, msg}, %{observers: obsrvs} = s) do
    Logger.debug("Received message(host=#{s.host}: #{inspect(msg, charlists: :as_lists)}")
    # TODO add checking message age and identyfying  
    if !:queue.is_empty(obsrvs) do
      {{:value, pid}, obsrvs2} = :queue.out(obsrvs)
      send(pid, msg)
      {:noreply, %{s | observers: obsrvs2}}
    else
      {:noreply, s}
    end
  end

  @impl true
  def handle_info({:tcp_closed, _port}, s) do
    Logger.info("Tcp port closed")
    {:stop, :normal, s}
  end

  @impl true
  def handle_call(_, _, %{sock: nil} = s) do
    {:reply, {:error, :closed}, s}
  end

  @impl true
  def handle_call({:send, data}, _from, %{sock: sock} = s) do
    case :gen_tcp.send(sock, data) do
      :ok ->
        {:reply, :ok, s}

      {:error, _} = error ->
        {:disconnect, error, error, s}
    end
  end

  @impl true
  def handle_call(
        {:confirm_send, data},
        {fpid, _},
        %{sock: sock, observers: observers} = s
      ) do
    case :gen_tcp.send(sock, data) do
      :ok ->
        obsrv = :queue.in(fpid, observers)
        {:reply, :ok, %{s | observers: obsrv}}

      {:error, _} = error ->
        {:disconnect, error, error, s}
    end
  end
end
