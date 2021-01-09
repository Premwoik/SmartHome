defmodule Ui.LogFormatter do
  @moduledoc false
  #  @protected [:request_id]

  def format(level, message, timestamp, metadata) do
    "##### #{fmt_timestamp(timestamp)} #{fmt_metadata(metadata)} [#{level}] --- #{message}\n"
  rescue
    _ -> "could not format message: #{inspect({level, message, timestamp, metadata})}\n"
  end

  defp fmt_metadata(md) do
    Enum.map(md, &output_metadata/1)
    |> Enum.join(" ")
  end

  defp output_metadata({:application, value}), do: "[#{value}]"
  defp output_metadata({key, value}), do: "#{key}=#{value}"

  defp fmt_timestamp({date, {hh, mm, ss, ms}}) do
    with {:ok, timestamp} <- NaiveDateTime.from_erl({date, {hh, mm, ss}}, {ms * 1000, 3}) do
      timestamp
    end
  end
end
