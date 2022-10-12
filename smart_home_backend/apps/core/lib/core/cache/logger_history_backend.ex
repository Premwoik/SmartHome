defmodule Core.LoggerHistoryBackend do
  @behaviour :gen_event

  alias DB.Series.Log
  alias DB.InfluxConnection

  @impl true
  def init(_) do
    {:ok, []}
  end

  @impl true
  def handle_event({level, _gl, {Logger, msg, _date, opts}}, state)
      when level in [:info, :warn, :error] do
    if Keyword.get(opts, :write_log, true) do
      app = Keyword.get(opts, :application, :unknown)
      InfluxConnection.write(Log.new(app, level, msg))
    end

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
