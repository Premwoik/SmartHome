defmodule Core.Utils.Time.Real do
  @moduledoc false

  @behaviour Core.Utils.Time

  @impl true
  def after?(a, b) do
    compare(a, b) == :gt
  end

  @impl true
  def before?(a, b) do
    compare(a, b) == :lt
  end

  @impl true
  def now() do
    Time.utc_now()
  end

  @impl true
  def compare(a, b) do
    Time.compare(a, b)
  end

  @impl true
  def datetime_now() do
    DateTime.utc_now()
  end

end
