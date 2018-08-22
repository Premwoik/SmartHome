defmodule Device.Client do

  @moduledoc false

  use Connection
  require Logger

  def start_link(host, port, opts, keywords, timeout \\ 5000, length \\ 11) do
    Logger.info("Initializing Device.Client")
    Connection.start_link(__MODULE__, {host, port, opts, timeout, length}, keywords)
  end


  ## Public
  def get_info, do:
    :server

  def confirmed_send(pid, msg) do
    Connection.call(pid, {:confirm_send, msg})
  end

  def send(pid, msg) do
    Connection.call(pid, {:send, msg})
  end


  ## Callbacks

  @impl true
  def init({host, port, opts, timeout, length}) do
    s = %{host: host, port: port, opts: opts, timeout: timeout, length: length, sock: nil, observers: :queue.new()}
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
        Logger.info "cannot connect"
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
  def handle_info({:tcp, port, msg}, %{observers: obsrvs} = s) do
    Logger.info("Received message: #{inspect(msg, charlists: :as_lists)}")

    if !:queue.is_empty(obsrvs) do
      {{:value, pid}, obsrvs2} = :queue.out(obsrvs)
      Kernel.send(pid, {:msg, msg})

      {:noreply, %{s | observers: obsrvs2}}
    else
      {:noreply, s}
    end
  end

  @impl true
  def handle_info({:tcp_closed, port}, s) do
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
        %{sock: sock, length: length, observers: observers} = s
      ) do
    case :gen_tcp.send(sock, data) do
      :ok ->
        obsrv = :queue.in fpid, observers
        {:reply, :ok, %{s | observers: obsrv}}
      {:error, _} = error ->
        {:disconnect, error, error, s}
    end
  end

  #  Privates


end
