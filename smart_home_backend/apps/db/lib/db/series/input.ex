defmodule DB.Series.Input do
  use Instream.Series

  series do
    measurement("input")

    tag(:device)
    tag(:pin)

    field(:value)
  end

  def new(device, pin, value) do
    data = %__MODULE__{}
    data = %{data | fields: %{data.fields | value: value}}
    %{data | tags: %{data.tags | device: device, pin: pin}}
  end
end
