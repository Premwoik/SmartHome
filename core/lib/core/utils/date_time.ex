defmodule Core.Utils.DateTime do
  @moduledoc false

  @type t :: NaiveDateTime.t

  @callback compare(t1 :: t, t2 :: t) :: atom
  @callback now() :: t
  @callback to_naive(t :: DateTime) :: t


  @adapter Application.get_env(:core, :date_time_adapter)

  @spec to_unix(t, integer) :: integer
  def to_unix(t, unit \\ :second) do
    {:ok, datetime} = DateTime.from_naive(t, "Etc/UTC")
    DateTime.to_unix(datetime, unit)
  end

  def to_unix_from_now(t, unit \\ :second) do
    :os.system_time(unit) - to_unix(t, unit)
  end

  def now_before?(t) do
    before?(now(), t)
  end

  def now_after?(t) do
    after?(now(), t)
  end

  @spec now() :: t
  def now() do
    @adapter.now()
  end

  def to_naive(t)do
    @adapter.to_naive(t)
  end

  @spec compare(t1 :: t, t2 :: t) :: atom
  def compare(t1, t2) do
    @adapter.compare(t1, t2)
  end


  @spec before?(t1 :: t, t2 :: t) :: boolean
  def before?(t1, t2) do
    @adapter.compare(t1, t2) == :lt
  end

  @spec after?(t1 :: t, t2 :: t) :: boolean
  def after?(t1, t2) do
    @adapter.compare(t1, t2) == :gt
  end


end
