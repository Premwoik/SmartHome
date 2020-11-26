defmodule RstpToWs.Camera.Converter do
  @moduledoc false

  use GenServer

  alias Porcelain.Process, as: Proc
  require Logger

  def start_link(state \\ []) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  def register_consumer(ref) do
    GenServer.cast(__MODULE__, {:register, ref})
  end

  def unregister_consumer(ref) do
    GenServer.cast(__MODULE__, {:remove, ref})
  end

  #  callbacks

  @impl true
  def init(_opts) do
    {
      :ok,
      %{
        consumers: MapSet.new(),
        close_timer: make_ref(),
        cameras: %{
          "cam1" => {
            "rtsp://192.168.2.130:554/user=admin&password=qqaazzxsw71&channel=1&stream=0.sdp?real_stream--rtp-caching=100",
            "9996",
            nil
          },
          "cam2" => {
            "rtsp://192.168.2.130:554/user=admin&password=qqaazzxsw71&channel=2&stream=0.sdp?real_stream--rtp-caching=100",
            "9997",
            nil
          },
          "cam3" => {
            "rtsp://192.168.2.130:554/user=admin&password=qqaazzxsw71&channel=3&stream=0.sdp?real_stream--rtp-caching=100",
            "9998",
            nil
          },
          "cam4" => {
            "rtsp://192.168.2.130:554/user=admin&password=qqaazzxsw71&channel=4&stream=0.sdp?real_stream--rtp-caching=100",
            "9999",
            nil
          },
        },
      }
    }
  end

  @impl true
  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:register, ref}, state) do
    state = inc(state, ref)
    size = MapSet.size(state.consumers)

    state = if size == 1 do
      res = Process.read_timer(state.close_timer)
      if res != false and res > 0 do
        Process.cancel_timer(state.close_timer)
        state
      else
        start_retransmission(state)
      end
    else
      state
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:remove, ref}, state) do
    state = dec(state, ref)
    size = MapSet.size(state.consumers)

    state = if size == 0 do
      ref = Process.send_after(self(), :end_signal, 15_000)
      %{state | close_timer: ref}
    else
      state
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(_msg, state) do
    Logger.error("Unhandled cast msg")
    {:noreply, state}
  end

  @impl true
  def handle_info(:end_signal, state) do
    state = end_retransmission(state)
    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.warn("Converter.handle_info: #{inspect msg}")
    {:noreply, state}
  end

  # privates

  defp dec(%{consumers: c} = state, ref) do
    %{state | consumers: MapSet.delete(c, ref)}
  end

  defp inc(%{consumers: c} = state, ref) do
    %{state | consumers: MapSet.put(c, ref)}
  end

  defp start_retransmission(%{cameras: cameras} = state) do
    Logger.info("Starting retransmitting the rstp to the ws!")

    cameras = Enum.map(cameras, fn {n, data} -> {n, spawn2(data)} end)
              |> Map.new()

    %{state | cameras: cameras}
  end

  defp spawn2({addr, port, nil}) do
    process = Porcelain.spawn(
      "node",
      ["/home/prw/CODE/SmartHome/rstp_to_ws/js/index.js", addr, port],
      in: :receive
    )
    {addr, port, process}
  end
  defp spawn2(d), do: d


  defp end_retransmission(%{cameras: cameras} = state) do
    Logger.info("Stopping retransmission!")
    cameras = Enum.map(cameras, fn {n, d} -> {n, kill(d)} end)
              |> Map.new()

    %{state | cameras: cameras}
  end

  defp kill({_, _, nil} = d), do: d
  defp kill({addr, port, proc}) do
    if Proc.alive?(proc) do
      Proc.signal(proc, 15)
      Proc.stop(proc)
    end
    {addr, port, nil}
  end
end
