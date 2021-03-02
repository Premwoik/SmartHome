defmodule UiWeb.DeviceJournalController do
  use UiWeb, :controller
  alias DB.Stats.DeviceJournal

  def get_logs(conn, %{"id" => id}) do
    data =
      DeviceJournal.get(id, _limit = 50)
      |> Enum.map(fn l -> log_mapper(l) end)

    json(conn, data)
  end

  defp log_mapper(l) do
    %{
      type: l.type,
      name: l.name,
      info: l.info,
      device_id: l.device_id,
      inserted_at: l.inserted_at
    }
  end

#  def get_report(conn, %{"id" => id}) do
#    data = DeviceJournal.report(id)
#    json(conn, data)
#  end
#
#  def get_report_s(conn, %{"id" => id}) do
#    data =
#      DeviceJournal.report_s(id)
#      |> Enum.reduce({nil, []}, fn x, {tmp, acc} -> {x, [fill_gaps(x, tmp) | acc]} end)
#      |> elem(1)
#      |> Enum.reverse()
#      |> Enum.concat()
#
#    json(conn, data)
#  end
#
#  defp fill_gaps(r1, nil) do
#    [r1]
#  end
#
#  defp fill_gaps(r2, r1) do
#    step = 5 * 60
#    num = round((r2.date - r1.date) / step) - 1
#
#    if num > 0 do
#      l = for i <- num..1, do: %{date: r1.date + i * step, value: 0}
#      Enum.reverse([r2 | l])
#    else
#      [r2]
#    end
#  end
end
