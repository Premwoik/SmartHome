defmodule Core.LoggerHistoryBackend do
  @behaviour :gen_event

  @impl true
  def init(_) do
    {:ok, []}
  end

  @impl true
  def handle_event({level, _gl, {Logger, msg, date, opts}}, state)
      when level in [:info, :warn, :error] do
    {:ok, true} = Cachex.put(:loggs_cache, date, {msg, opts})
    {:ok, state}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  @impl true
  def terminate(_, _) do
    :ok
  end
end
