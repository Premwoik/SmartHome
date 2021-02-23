defmodule Core.Utils.Time do
  @moduledoc false

  use Timex

  @spec in_interval?(Date.t(), Date.t()) :: boolean
  def in_interval?(from, to) do
    actual = DateTime.to_time(Timex.local())

    case Time.compare(to, from) do
      :lt ->
        Time.compare(actual, from) == :gt || Time.compare(to, actual) == :gt

      :gt ->
        Time.compare(actual, from) == Time.compare(to, actual)

      _ ->
        false
    end
  end
end
