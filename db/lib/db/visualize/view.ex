defmodule DB.Visualize.View do
  @moduledoc false

  def display([%{__struct__: module} | _] = records) do
    attrs = module.__info__.attributes
    separator = " | "
    sizes = get_column_length(attrs, records)
    print_header_view(attrs, separator, sizes)
    Enum.map(records, & print_record_view(&1, attrs, separator, sizes))
    :ok
  end

  defp print_header_view(attrs, separator, sizes) do
    names = Enum.reduce(attrs, "", fn attr, acc ->
      size = Map.get(sizes, attr)
      space = String.duplicate(" ", abs(round((size -  String.length(to_string(attr))) / 2)))
      if acc == "", do: acc <> to_string(attr), else: acc <> separator <> space <> to_string(attr) <> space
    end)
    IO.puts(names)
  end

  defp print_record_view(item, attrs, separator, sizes) do
    Enum.reduce(attrs, "", fn attr, acc ->
      size = Map.get(sizes, attr)
      space = String.duplicate(" ", abs(round((size -  col_length(Map.get(item, attr))) / 2)))
      view = render_attribute(Map.get(item, attr))
      case acc do
        "" -> acc <> view <> space
        _ -> acc <> separator <> space <> view <> space
      end
    end)
    |> IO.puts()
  end

  defp render_attribute(x) when is_map(x), do: "%{...}"
  defp render_attribute(x) when is_list(x), do: "[...]"
  defp render_attribute(x) when is_tuple(x), do: "#{inspect x}"
  defp render_attribute(nil), do: "NULL"
  defp render_attribute(attr), do: to_string(attr)


  defp get_column_length(attrs, records) do
    acc = Enum.reduce(attrs, %{}, fn attr, acc ->
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

  defp col_length(x) when is_map(x), do: 6
  defp col_length(x) when is_list(x), do: 5
  defp col_length(x) when is_tuple(x), do: String.length("#{inspect x}")
  defp col_length(nil), do: 4
  defp col_length(attr), do: String.length(to_string(attr))



end
