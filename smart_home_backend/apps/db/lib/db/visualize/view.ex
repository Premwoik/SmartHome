defmodule DB.Visualize.View do
  @moduledoc false

  defmodule Print do
    def display(records), do: DB.Visualize.View.display(records) |> IO.puts()
  end

  def display([%{} = sample | _] = records) do
    attrs = get_attrs(sample)
    separator = " | "
    sizes = get_column_length(attrs, records)
    header = print_header_view(attrs, separator, sizes)
    records = Enum.map(records, &print_record_view(&1, attrs, separator, sizes))

    [header | records]
    |> Enum.join("\n")
  end

  def display(record), do: List.wrap(record) |> display()

  defp get_attrs(%{__struct__: module}), do: module.__info__.attributes
  defp get_attrs(%{} = sample), do: Map.keys(sample)

  defp print_header_view(attrs, separator, sizes) do
    Enum.reduce(attrs, "", fn attr, acc ->
      size = Map.get(sizes, attr)
      item_size = String.length(to_string(attr))
      space = String.duplicate(" ", abs(round((size - item_size) / 2)))

      if acc == "" do
        String.slice(space <> to_string(attr) <> space, 0, size)
      else
        acc <> separator <> String.slice(space <> to_string(attr) <> space, 0, size)
      end
    end)
  end

  defp print_record_view(item, attrs, separator, sizes) do
    Enum.reduce(attrs, "", fn attr, acc ->
      size = Map.get(sizes, attr)
      item_size = col_length(Map.get(item, attr))
      space = String.duplicate(" ", abs(round((size - item_size) / 2)))
      view = render_attribute(Map.get(item, attr))

      case acc do
        "" -> String.slice(space <> view <> space, 0, size)
        _ -> acc <> separator <> String.slice(space <> view <> space, 0, size)
      end
    end)

    #    |> IO.puts()
  end

  defp render_attribute(x) when is_list(x), do: "[...]"
  defp render_attribute(x) when is_map(x), do: "#{inspect(x)}"
  defp render_attribute(x) when is_tuple(x), do: "#{inspect(x)}"
  defp render_attribute(nil), do: "NULL"
  defp render_attribute(attr), do: to_string(attr)

  defp get_column_length(attrs, records) do
    acc =
      Enum.reduce(attrs, %{}, fn attr, acc ->
        size = String.length(to_string(attr))
        #      size = if(rem(size, 2) > 0, do: size + 1, else: size)
        Map.put(acc, attr, size)
      end)

    Enum.reduce(records, acc, fn r, acc ->
      Enum.reduce(attrs, acc, fn attr, acc ->
        size = col_length(Map.get(r, attr))

        if Map.get(acc, attr, 0) < size do
          #          size = if(rem(size, 2) > 0, do:  size + 1, else: size)
          Map.put(acc, attr, size)
        else
          acc
        end
      end)
    end)
  end

  defp col_length(x) when is_list(x), do: 5
  defp col_length(x) when is_map(x), do: String.length("#{inspect(x)}")
  defp col_length(x) when is_tuple(x), do: String.length("#{inspect(x)}")
  defp col_length(nil), do: 4
  defp col_length(attr), do: String.length(to_string(attr))
end
