defmodule DB.DeviceActivations do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Query
  import DB.Activations

  alias DB.{DeviceJournal, Device, DeviceActivations, Repo, Activations}

  @behaviour Activations

  schema "devices_activations" do
    belongs_to(:device, Device)
    field(:date, :naive_datetime)
    field(:value, :integer)
    field(:infos, :string)
  end

  def find(id, limit \\ 24) do
    from(
      i in DeviceActivations,
      where: i.device_id == ^id,
      order_by: [
        desc: i.date
      ],
      limit: ^limit
    )
    |> Repo.all()
  end

  def collect_previous_hour() do
    collect_previous_hour_(DeviceActivations, &collect_data/2)
  end

  defp collect_data(from, to) do
    from(
      l in DeviceJournal,
      where: l.inserted_at >= ^from and l.inserted_at < ^to
    )
    |> Repo.all()
    |> Enum.group_by(fn log -> log.device_id end)
    |> Enum.map(fn {key, value} ->
      num = Enum.count(value)
      stacked_info = stack_info(value)

      %DeviceActivations{
        device_id: key,
        date: from,
        value: num,
        infos: stacked_info
      }
      |> Repo.insert()
    end)
  end

  @spec stack_info(list(%DeviceJournal{})) :: :string
  def stack_info(is) do
    is
    |> Enum.map(fn i -> "#{i.name} - #{i.info}}" end)
    |> Enum.join("\n")
  end
end
