defmodule DB.Series.Log do
  use Instream.Series

  alias DB.Series.Log

  series do
    measurement("elixir_log")

    tag(:app)
    tag(:level)

    field(:msg)
  end

  def new(app, level, msg) do
    data = %Log{}
    data = %{data | fields: %{data.fields | msg: msg}}
    %{data | tags: %{data.tags | app: app, level: level}}
  end
end
