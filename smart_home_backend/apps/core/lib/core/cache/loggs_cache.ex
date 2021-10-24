defmodule Core.Cache.LoggsCache do
  @cache_name :loggs_cache
  @pattern Cachex.Query.create(true, {:key, :value})

  def get_latest(num \\ 100) do
    Cachex.stream!(@cache_name, @pattern)
    |> Enum.to_list()
    |> Enum.sort_by(&elem(&1, 0), &(&1 > &2))
    |> Enum.reverse()
    |> Enum.take(num)
  end

  def get_page(page, num \\ 100) do
    Cachex.stream!(@cache_name, @pattern)
    |> Enum.to_list()
    |> Enum.sort_by(&elem(&1, 0), &(&1 > &2))
    |> Enum.drop(page * num)
    |> Enum.take(num)
  end
end
