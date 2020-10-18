defmodule DB.Activations do
  @moduledoc false

  alias DB.{Repo, InputActivations, DeviceActivations, OutputActivations}
  import Ecto.Changeset
  import Ecto.Query

  @callback collect_previous_hour() :: any()

  @spec collect_previous_hour_(mod :: module(), collector_fn :: (DateTime.t(), DateTime.t() -> any())) :: any()
  def collect_previous_hour_(mod, collector_fn) do
    latest_read_date = get_latest_date(mod)
    current_read_date = next_date(latest_read_date)
    current_read_date_end = next_date(current_read_date)
    date = clear_to_hour(NaiveDateTime.utc_now())
    if date > current_read_date do
      collector_fn.(current_read_date, current_read_date_end)
    end
  end

  defp get_latest_date(mod) do
    last_date =
      from(
        x in mod,
        order_by: [
          desc: x.id
        ],
        limit: 1,
        select: x.date
      )
      |> Repo.one()
    if last_date == nil, do: prev_date(prev_date(clear_to_hour(NaiveDateTime.utc_now()))), else: last_date
  end

  defp prev_date(dt) do
    hour_ = case dt.hour do
      0 -> 23
      h -> h - 1
    end
    %{dt | hour: hour_}
  end

  defp next_date(dt) do
    hour_ = case dt.hour do
      23 -> 0
      h -> h + 1
    end
    %{dt | hour: hour_}
  end

  defp clear_to_hour(dt) do
    %{dt | minute: 0, second: 0, microsecond: {0, 0}}
  end

  @spec init() :: any()
  def init() do
    [
      %DB.DeviceJournal{
        device_id: 2,
        id: 1,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 2,
        info: "[8,9,6]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 3,
        info: "[8,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 4,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 5,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 6,
        info: "[14,6,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 7,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 8,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      },
      %DB.DeviceJournal{
        device_id: 2,
        id: 9,
        info: "[14,15,16]",
        name: "read_active_inputs",
        type: "NORMAL",
      }
    ]
    |> Enum.map(&(Repo.insert(&1)))
  end
end
