defmodule Core.Utils.Time do
  @moduledoc false

  @callback now() :: Time.t
  @callback compare(t1 :: Time.t, t2 :: Time.t) :: atom



  @adapter Application.get_env(:core, :time_adapter)


  def now() do
    @adapter.now()
  end

  def compare(a, b) do
    @adapter.compare(a, b)
  end

  def in_interval?(n \\ now(), s, e) do
    case compare(e, s) do
      :lt ->
        compare(n, s) == :gt || compare(e, n) == :gt
      :gt ->
        compare(n, s) == compare(e, n)
      _ -> false
    end
  end
end
