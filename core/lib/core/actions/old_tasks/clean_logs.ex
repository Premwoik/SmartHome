defmodule Core.Actions.CleanLogs do
  @moduledoc false

  @behaviour Core.Actions.Action
  alias DB.Stats.{DeviceJournal}

  @impl true
  def execute(_on_off, _action, date) do
    DeviceJournal.delete_older(date)
    {:ok, NaiveDateTime.utc_now()}
  end

  @impl true
  def init_state() do
    NaiveDateTime.utc_now()
  end
end
