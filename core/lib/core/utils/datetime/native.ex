defmodule Core.Utils.Datetime.Native do
  @moduledoc false

  @behaviour Core.Utils.DateTime


  @impl true
  def compare(t1, t2) do
    NaiveDateTime.compare(t1, t2)
  end

  @impl true
  def now() do
    NaiveDateTime.utc_now()
  end

  @impl true
  def to_naive(t) do
    DateTime.to_naive(t)
  end

end
