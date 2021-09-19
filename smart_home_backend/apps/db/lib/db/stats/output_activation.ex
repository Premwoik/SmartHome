defmodule DB.Stats.OutputActivation do
  @moduledoc false

  use DB.Stats.Data, module: __MODULE__, owner_id_field: :port_id

  defmodule Schema do
    defmacro __using__(name) do
      quote do
        use Ecto.Schema
        @name unquote(name)
        @derive {Poison.Encoder, except: [:__meta__]}
        schema @name do
          #      field(:device_id, :integer)
          field(:port_id, :integer)
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
    use DB.Stats.OutputActivation.Schema, "output_activations_1h"
  end

  defmodule OneDay do
    use DB.Stats.OutputActivation.Schema, "output_activations_1day"
  end

  defmodule OneWeek do
    use DB.Stats.OutputActivation.Schema, "output_activations_1week"
  end

  defmodule OneMonth do
    use DB.Stats.OutputActivation.Schema, "output_activations_1month"
  end

  def collect(:hourly = precision) do
    source_module = get_source_mod(__MODULE__, precision, DB.Stats.DeviceJournal)
    now = get_now_date(precision)
    from = from_date(now, precision)

    source_module.collect("set_outputs", from, now)
    |> Enum.map(fn %{arguments: args} -> Poison.decode!(args) end)
    |> Enum.concat()
    |> Enum.reduce(%{}, fn %{"id" => id}, acc -> Map.update(acc, id, 1, &(&1 + 1)) end)
    |> Enum.each(fn {id, value} ->
      add(%{port_id: id, date: from, value: value}, precision)
    end)
  end

  def collect(precision) do
    source_module = get_source_mod(__MODULE__, precision, DB.Stats.DeviceJournal)
    now = get_now_date(precision)
    from = from_date(now, precision)

    source_module.collect(nil, from, now)
    |> Enum.reduce(%{}, fn %{port_id: id, value: v}, acc -> Map.update(acc, id, v, &(&1 + v)) end)
    |> Enum.each(fn {id, value} ->
      add(%{port_id: id, date: from, value: value}, precision)
    end)
  end
end
