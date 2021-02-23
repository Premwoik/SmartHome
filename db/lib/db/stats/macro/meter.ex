defmodule DB.Stats.Meter do
  @moduledoc false
  defmacro __using__(_) do
    quote do
      defmodule Reading do
        use Ecto.Schema
        @derive {Poison.Encoder, except: [:__meta__]}
        schema "energy_readings" do
          field(:meter_id, :integer)
          field(:value, :integer)
          timestamps()
        end

        def collect(from, to) do
          __MODULE__
          |> set_from_t(from)
          |> set_to_t(to)
          |> DB.StatsRepo.all()
        end
      end

      def collect(precision) do
        module = add_precision_to_mod(__MODULE__, precision)
        now = clear_to_precision(NaiveDateTime.utc_now(), precision)
        from = from_date(now, precision)

        __MODULE__.Reading.collect(from, now)
        |> Enum.group_by(& &1.meter_id, & &1.value)
        |> Enum.each(fn {meter_id, values} ->
          avg_val = Enum.sum(values) / Enum.count(values)
          module.add(%{meter_id: meter_id, date: from, value: avg_val}, precision)
        end)
      end
    end
  end
end
