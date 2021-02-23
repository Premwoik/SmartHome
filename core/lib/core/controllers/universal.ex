defmodule Core.Controllers.Universal do
  @moduledoc false

  alias DB.{Port}

  @type port_child :: %{required(:port) => %Port{}}
  @type res :: :ok | {:error, any(), any()}
  @type item :: %Port{}

  @spec filter_invalid(list(item), list(item)) :: list(item)
  def filter_invalid(items, invalid) do
    Enum.filter(items, fn x -> !Enum.any?(invalid, fn x1 -> items_match?(x, x1) end) end)
  end

  @spec get_passed_items(res(), list(item)) :: list(item)
  def get_passed_items(:ok, items), do: items
  def get_passed_items({:error, invalid, _}, items), do: filter_invalid(items, invalid)

  @spec flatten_result(list(res())) :: res()
  def flatten_result(list) do
    errors = Enum.filter(list, &(&1 != :ok))

    if length(errors) > 0 do
      Enum.reduce(
        errors,
        fn {:error, p1, err1}, {:error, p2, err2} ->
          {:error, p1 ++ p2, err1 ++ err2}
        end
      )
    else
      :ok
    end
  end

  # Privates

  defp items_match?(item, invalid_item) do
    case {item, invalid_item} do
      {%Port{}, %Port{}} -> item.id == invalid_item.id
      {_, %Port{}} -> item.port.id == invalid_item.id
      _ -> item == invalid_item
    end
  end
end
