defmodule DB.DeviceJournal do
  use Ecto.Schema
  @moduledoc false
  import Ecto.Changeset
  import Ecto.Query
  alias DB.{DeviceJournal, Device, Repo}
  alias DB.DeviceJournal.Type

  defmodule Type do
    def normal, do: "NORMAL"
    def error, do: "ERROR"
    def timeout, do: "TIMEOUT"
  end

  defmodule Name do
    def read_active_inputs, do: "read_active_inputs"
#    def
  end

  schema "device_journals" do
    field(:type, :string)
    field(:name, :string)
    field(:info, :string)
    belongs_to(:device, DB.Device)
    timestamps
  end

  @spec log(%DB.Device{} | integer(), string(), string()) :: any()
  def log(device, name, info \\ "", type \\ "NORMAL")

  def log(%Device{} = d, name, info, type) do
    log(d.id, name, info, type)
  end

  def log(device_id, name, info, type) do
    Task.start(
      fn ->
        %DeviceJournal{
          type: type,
          name: name,
          info: info,
          device_id: device_id
        }
        |> Repo.insert!()
      end
    )
  end

  def find(device, limit \\ 1000, from \\ nil)

  def find(%Device{} = d, limit, from) do
    find(d.id, from, limit)
  end

  def find(device_id, limit, nil) do
    Repo.all(
      from(
        d in DeviceJournal,
        where: d.device_id == ^device_id,
        order_by: [
          desc: d.inserted_at
        ],
        limit: ^limit
      )
    )
  end

  def find(device_id, limit, from) do
    Repo.all(
      from(
        d in DeviceJournal,
        where: d.device_id == ^device_id and d.inserted_at <= ^from,
        order_by: [
          desc: d.inserted_at
        ],
        limit: ^limit
      )
    )
  end

  def delete_older(date) do
    from(d in DeviceJournal, where: d.inserted_at <= ^date)
    |> Repo.delete_all()
  end

  def report(device_id, hours \\ 3) do
    seconds = round(hours * 3600)

    from(
      d in DeviceJournal,
      where:
        d.device_id == ^device_id and
        fragment(
          "cast(strftime('%s', ?) as Integer) >= strftime('%s', 'now') - ?",
          d.inserted_at,
          ^seconds
        ),
      group_by:
        fragment(
          "cast((julianday('2019-01-01T00:00:00') - julianday(?))*24*60 As Integer) / 5",
          d.inserted_at
        ),
      select: %{
        value: count(d.id),
        date:
          fragment(
            "datetime(strftime('%s', ?) - strftime('%s', ?) % 300, 'unixepoch', 'localtime')",
            d.inserted_at,
            d.inserted_at
          )
      }
    )
    |> Repo.all()
  end

  def report_s(device_id, hours \\ 3) do
    seconds = round(hours * 3600)

    from(
      d in DeviceJournal,
      where:
        d.device_id == ^device_id and
        fragment(
          "cast(strftime('%s', ?) as Integer) >= strftime('%s', 'now') - ?",
          d.inserted_at,
          ^seconds
        ),
      group_by:
        fragment(
          "cast((julianday('2019-01-01T00:00:00') - julianday(?))*24*60 As Integer) / 5",
          d.inserted_at
        ),
      select: %{
        value: count(d.id),
        date:
          fragment(
            "strftime('%s', 'now') - (strftime('%s', ?) - strftime('%s', ?) % 300)",
            d.inserted_at,
            d.inserted_at
          )
      }
    )
    |> Repo.all()
  end

  def delete_older_than(hours \\ 3) do
    seconds = round(hours * 3600)
    from(
      d in DeviceJournal,
      where: fragment("strftime('%s', 'now') - ? < strftime('%s', ?)", ^seconds, d.inserted_at)
    )
    |> Repo.delete_all()
  end
end
