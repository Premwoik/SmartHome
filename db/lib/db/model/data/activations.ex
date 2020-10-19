defmodule DB.Activations do
  @moduledoc false

  alias DB.{Repo, InputActivations, DeviceActivations, OutputActivations}
  import Ecto.Changeset
  import Ecto.Query

  @callback collect_previous_hour() :: any()

  @spec collect_previous_hour_(mod :: module(), collector_fn :: (DateTime.t(), DateTime.t() -> any())) :: any()
  def collect_previous_hour_(mod, collector_fn) do
    latest_read_date = get_latest_date(mod)
    current_date = clear_to_hour(NaiveDateTime.utc_now())
    prev_list(last_read_date, current_date)
    |> Enum.map(fn {from, to} -> collector_fn.(from, to) end)
    |> Enum.concat()
  end

  defp get_prev_hours_list(latest_read_date, current_date, acc \\ [])
  defp get_prev_hours_list(latest_read_date, current_date, acc) when latest_read_date < current_date do
    from = next_date(latest_read_date)
    to = next_date(from)
    acc_ = [{from, to} | acc]
    get_prev_hours_list(from, current_date, acc_)
  end
  defp prev_list(_, _, acc) do
    Enum.reverse(acc)
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