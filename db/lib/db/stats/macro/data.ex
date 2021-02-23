defmodule DB.Stats.Data do
  @moduledoc false

  import Ecto.Query

  @type(precision() :: :hourly, :daily, :weekly, :monthly)

  @callback add(data :: map(), precision :: precision()) :: :ok
  @callback get(
              id :: integer(),
              precision :: precision(),
              opts :: keyword()
            ) :: list()
  @callback all(
              from :: NaiveDateTime.t(),
              to :: NaiveDateTime.t(),
              precision :: precision()
            ) :: list()
  @callback collect(precision :: precision()) :: list()

  defmacro __using__(opts) do
    quote do
      import Ecto.Query
      import DB.Stats.Data
      @behaviour DB.Stats.Data

      @opts unquote(opts)
      @module Keyword.get(@opts, :module, DB.Stats.Data)
      @repo Keyword.get(@opts, :repo, DB.StatsRepo)
      @owner_id_field Keyword.get(@opts, :owner_id_field, :owner_id)

      def add(data, precision) do
        module = add_precision_to_mod(@module, precision)
        DB.StatsRepo.insert(struct(module, data))
      end

      #      def collect(id, precision) do
      #        module = add_precision_to_mod(@module, precision)
      #        now = clear_to_precision(NaiveDateTime.utc_now(), precision)
      #        from = from_date(now, precision)
      #
      #        module
      #        |> where([d], d.inserted_at >= from and d.inserted_at < now)
      #        |> select([d], count(d))
      #      end

      def get(id, precision, opts \\ [])

      def get(id, precision, opts) do
        module = add_precision_to_mod(@module, precision)
        from = Keyword.get(opts, :from, nil)
        to = Keyword.get(opts, :to, nil)
        limit_ = Keyword.get(opts, :limit, nil)
        cond = Keyword.get(opts, :where, []) |> Keyword.put(@owner_id_field, id)
        module |> where(^cond) |> set_from(from) |> set_to(to) |> set_limit(limit_) |> @repo.all()
      end

      def all(from, to, precision \\ :daily)

      def all(from, to, precision) do
        module = add_precision_to_mod(@module, precision)
        module |> set_from(from) |> set_to(to) |> @repo.all()
      end

      #      defoverridable add: 2, get: 5, all: 4
    end
  end

  #  defmodule Collector do
  #    def collect(precision) do
  #      DB.Stats.DeviceActivation.collect(precision)
  #      DB.Stats.OutputActivation.collect(precision)
  #      DB.Stats.InputActivation.collect(precision)
  #      DB.Stats.Temperature.collect(precision)
  #      DB.Stats.Energy.collect(precision)
  #    end
  #  end

  def set_from(query, nil), do: query
  def set_from(query, from), do: where(query, [d], d.date >= ^from)

  def set_to(query, nil), do: query
  def set_to(query, to), do: where(query, [d], d.date < ^to)

  def set_from_t(query, nil), do: query
  def set_from_t(query, from), do: where(query, [d], d.inserted_at >= ^from)

  def set_to_t(query, nil), do: query
  def set_to_t(query, to), do: where(query, [d], d.inserted_at < ^to)

  def set_limit(query, nil), do: query
  def set_limit(query, l), do: limit(query, ^l)

  @spec get_now_date(precision, DateTime.t()) :: NaiveDateTime.t()
  def get_now_date(precision, date \\ Timex.local()) do
    clear_to_precision(date, precision) |> Timex.to_naive_datetime()
  end

  def clear_to_precision(date, precision) do
    case precision do
      :hourly ->
        %{date | minute: 0, second: 0, microsecond: {0, 0}}

      :daily ->
        %{date | hour: 0, minute: 0, second: 0, microsecond: {0, 0}}

      :weekly ->
        %{date | hour: 0, minute: 0, second: 0, microsecond: {0, 0}}

      :monthly ->
        %{date | day: 0, hour: 0, minute: 0, second: 0, microsecond: {0, 0}}
    end
  end

  def from_date(dt, precision) do
    case precision do
      :hourly ->
        Timex.shift(dt, hours: -1)

      :daily ->
        Timex.shift(dt, days: -1)

      :weekly ->
        Timex.shift(dt, weeks: -1)

      :monthly ->
        Timex.shift(dt, months: -1)
    end
  end

  def add_precision_to_mod(module, precision) do
    module = module |> to_string()

    case precision do
      :hourly -> module <> ".OneHour"
      :daily -> module <> ".OneDay"
      :weekly -> module <> ".OneWeek"
      :monthly -> module <> ".OneMonth"
    end
    |> String.to_existing_atom()
  end

  def get_source_mod(module, precision, source) do
    source = source |> to_string()
    module = module |> to_string()

    case precision do
      :hourly -> source
      :daily -> module <> ".OneHour"
      :weekly -> module <> ".OneDay"
      :monthly -> module <> ".OneWeek"
    end
    |> String.to_existing_atom()
  end
end
