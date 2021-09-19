defmodule DB.Stats.DeviceActivation do
  @moduledoc false

  use DB.Stats.Data, module: __MODULE__, owner_id_field: :device_id

  defmodule Schema do
    defmacro __using__(name) do
      quote do
        use Ecto.Schema
        @name unquote(name)
        @derive {Poison.Encoder, except: [:__meta__]}
        schema @name do
          field(:device_id, :integer)
          field(:date, :naive_datetime)
          field(:value, :integer)
          #          field(:infos, :string)
        end

        def collect(_, from, to) do
          __MODULE__
          |> set_from(from)
          |> set_to(to)
          |> DB.StatsRepo.all()
        end
      end
    end
  end

  defmodule OneHour do
    use DB.Stats.DeviceActivation.Schema, "device_activations_1h"
  end

  defmodule OneDay do
    use DB.Stats.DeviceActivation.Schema, "device_activations_1day"
  end

  defmodule OneWeek do
    use DB.Stats.DeviceActivation.Schema, "device_activations_1week"
  end

  defmodule OneMonth do
    use DB.Stats.DeviceActivation.Schema, "device_activations_1month"
  end

  def collect(:hourly = precision) do
    source_module = get_source_mod(__MODULE__, precision, DB.Stats.DeviceJournal)
    now = get_now_date(precision)
    from = from_date(now, precision)

    source_module.collect(nil, from, now)
    |> Enum.reduce(%{}, fn d, acc -> Map.update(acc, d.device_id, 1, &(&1 + 1)) end)
    |> Enum.each(fn {id, value} ->
      add(%{device_id: id, date: from, value: value}, precision)
    end)
  end

  def collect(precision) do
    source_module = get_source_mod(__MODULE__, precision, DB.Stats.DeviceJournal)
    now = get_now_date(precision)
    from = from_date(now, precision)

    source_module.collect(nil, from, now)
    |> Enum.reduce(%{}, fn %{device_id: id, value: v}, acc ->
      Map.update(acc, id, v, &(&1 + v))
    end)
    |> Enum.each(fn {id, value} ->
      add(%{device_id: id, date: from, value: value}, precision)
    end)
  end

  #  def get(device_id, from, to, precision, limit_ \\ nil) do
  #    add_precision_to_mod(__MODULE__, precision)
  #    |> set_to_t(to)
  #    |> set_from_t(from)
  #    |> where([r], r.device_id == ^device_id)
  #    |> set_limit(limit_)
  #    |> DB.StatsRepo.all()
  #  end
end
