defmodule DB.OutputActivations do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Query
  import DB.Activations
  alias DB.{DeviceJournal, Repo, Activations, OutputActivations}

  @behaviour Activations

  schema "outputs_activations" do
    belongs_to(:device, DB.Device)
    belongs_to(:port, DB.Port)
    field(:date, :naive_datetime)
    field(:value, :integer)
  end

  def find(id, limit \\ 24) do
    from(
      i in OutputActivations,
      where: i.port_id == ^id,
      order_by: [
        desc: i.date
      ],
      limit: ^limit
    )
    |> Repo.all()
  end

  def collect_previous_hour() do
    collect_previous_hour_(OutputActivations, &collect_data/2)
  end

  defp collect_data(from, to) do
    from(
      l in DeviceJournal,
      where: l.name == "set_outputs" and l.inserted_at >= ^from and l.inserted_at < ^to
    )
    |> Repo.all()
    |> Enum.group_by(fn log -> log.device_id end)
    |> Enum.map(fn {key, value} ->
      map_numbers(
        from,
        key,
        Enum.map(
          value,
          fn v ->
            String.split(v.info, "[", parts: 2)
            |> List.last()
            |> (&("[" <> &1)).()
            |> Poison.decode!()
          end
        )
        |> Enum.concat()
      )
    end)
    |> Enum.concat()
  end

  defp map_numbers(date, device_id, port_numbers) do
    ns =
      port_numbers
      |> Enum.with_index()
      |> Enum.filter(fn {_, i} -> rem(i, 2) == 0 end)
      |> Enum.map(fn {n, _} -> n end)
      |> Enum.reduce(%{}, fn x, acc -> Map.update(acc, x, 1, &(&1 + 1)) end)
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Map.new()

    keys = Map.keys(ns)

    from(
      p in DB.Port,
      where: p.number in ^keys and p.device_id == ^device_id,
      select: p.id,
      order_by: p.number
    )
    |> Repo.all()
    |> Enum.zip(ns)
    |> Enum.map(fn {p_id, {_, value}} ->
      %OutputActivations{
        port_id: p_id,
        device_id: device_id,
        date: date,
        value: value
      }
      |> Repo.insert()
    end)
  end
end
