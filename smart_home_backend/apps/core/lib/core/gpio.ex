defmodule Core.GPIO do
  use GenServer
  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def beep(pin \\ 18) do
    GenServer.cast(__MODULE__, {:beep, pin})
  end

  def write(pin, state) do
    GenServer.call(__MODULE__, {:write, pin, state})
  end

  def init(_) do
    {:ok, buzzer_ref} = Circuits.GPIO.open(18, :output)
    Circuits.GPIO.write(buzzer_ref, 1)
    {:ok, %{18 => buzzer_ref}}
  end

  def handle_call({:write, pin, val}, _from, state) do
    case Map.fetch(state, pin) do
      {:ok, ref} ->
        Circuits.GPIO.write(ref, val)
        {:reply, :ok, state}

      _ ->
        {:reply, :error, state}
    end
  end

  def handle_cast({:beep, pin}, state) do
    handle_call({:write, pin, 0}, self(), state)
    Process.send_after(self(), {:write, pin, 1}, 300)
    {:noreply, state}
  end

  def handle_cast(request, state) do
    Logger.error("Unsupported request #{inspect(request)}")
    {:noreply, state}
  end

  def handle_info({:write, _, _} = data, state) do
    {:reply, _, state} = handle_call(data, self(), state)
    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.error("Unsupported msg #{inspect(msg)}")
    {:noreply, state}
  end
end
