defmodule Core.Tasks.CleanLogs do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias DB.{DeviceJournal}

  @impl true
  def execute(_task, date) do
    DeviceJournal.delete_older(date)
    {:ok, NaiveDateTime.utc_now()}
  end

  @impl true
  def init_state() do
    NaiveDateTime.utc_now()
  end
end
