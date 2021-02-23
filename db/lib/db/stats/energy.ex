defmodule DB.Stats.Energy do
  @moduledoc false
  use DB.Stats.Data, module: __MODULE__, owner_id_field: :meter_id

  defmodule Reading do
    use Ecto.Schema
    @derive {Poison.Encoder, except: [:__meta__]}
    schema "energy_readings" do
      field(:meter_id, :integer)
      field(:value, :integer)
      timestamps()
    end

    def collect(_, from, to) do
      __MODULE__
      |> set_from_t(from)
      |> set_to_t(to)
      |> DB.StatsRepo.all()
    end
  end

  defmodule Schema do
    defmacro __using__(name) do
      quote do
        use Ecto.Schema
        @name unquote(name)
        @derive {Poison.Encoder, except: [:__meta__]}
        schema @name do
          field(:meter_id, :integer)
          field(:date, :naive_datetime)
          field(:value, :integer)
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
    use DB.Stats.Energy.Schema, "energy_1h"
  end

  defmodule OneDay do
    use DB.Stats.Energy.Schema, "energy_1day"
  end

  defmodule OneWeek do
    use DB.Stats.Energy.Schema, "energy_1week"
  end

  defmodule OneMonth do
    use DB.Stats.Energy.Schema, "energy_1month"
  end

  def collect(precision) do
    #    module = add_precision_to_mod(__MODULE__, precision)
    source_module = get_source_mod(__MODULE__, precision, __MODULE__.Reading)
    now = clear_to_precision(NaiveDateTime.utc_now(), precision)
    from = from_date(now, precision)

    source_module.collect(nil, from, now)
    |> Enum.group_by(& &1.meter_id, & &1.value)
    |> Enum.each(fn {meter_id, values} ->
      avg_val = Enum.sum(values) / Enum.count(values)
      add(%{meter_id: meter_id, date: from, value: avg_val}, precision)
    end)
  end

  #  def get(meter_id, from, to, precision, limit_ \\ nil) do
  #    add_precision_to_mod(__MODULE__, precision)
  #    |> set_to_t(to)
  #    |> set_from_t(from)
  #    |> where([r], r.meter_id == ^meter_id)
  #    |> set_limit(limit_)
  #    |> DB.StatsRepo.all()
  #  end
end
