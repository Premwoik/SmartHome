defmodule DB.Repo do
  @moduledoc false


  def get(mod, id) do
    id = if(is_binary(id), do: String.to_integer(id), else: id)
    mod.get(id)
  end

  def all(mod) do
    mod.all()
  end

  def insert(%{__struct__: mod} = struct) do
    mod.insert(struct)
  end

  def update(%{__struct__: mod} = struct) do
    mod.update(struct)
  end

  def delete(mod, id) when is_integer(id) do
    Memento.Query.delete(mod, id)
  end
  def delete(mod, id) when is_binary(id) do
    id = String.to_integer(id)
    Memento.Query.delete(mod, id)
  end
  def delete(%{__struct__: _} = struct) do
    Memento.transaction( fn ->
      Memento.Query.delete_record(struct)
    end
    )
  end

  @spec match?(any, any) :: boolean
  def match?({:foreign, s1, id1}, %{__struct__: s2, id: id2}), do: s1 == s2 and id1 == id2
  def match?(_, _), do: false

  def id(id) when is_integer(id), do: id
  def id({:foreign, _, id}), do: id
  def id(_), do: raise("Given param is wrong type!")

  def preload({:foreign, mod, id}) do
    mod.get(id)
  end

  def preload(items) when is_list(items) do
    Enum.map(items, &preload/1)
  end

  def preload(%{} = item) do
    enum_item = if(Map.has_key?(item, :__struct__), do: Map.from_struct(item), else: item)
    Enum.reduce(enum_item, item, &load_foreign/2)
  end

  def preload(x), do: x

  # Privates

  defp load_foreign({key, {:foreign, mod, id}}, acc) do
    Map.put(acc, key, mod.get(id))
  end

  defp load_foreign({key, val}, acc) when is_map(val) do
    Map.put(acc, key, preload(val))
  end

  defp load_foreign({key, val}, acc) when is_list(val) do
    Map.put(acc, key, Enum.map(val, &load_foreign_list/1))
  end

  defp load_foreign(_, acc), do: acc

  defp load_foreign_list({key, val}) do
    {key, load_foreign_list(val)}
  end

  defp load_foreign_list({:foreign, mod, id}) do
    mod.get(id)
  end

  defp load_foreign_list(val) when is_map(val) do
    preload(val)
  end

  defp load_foreign_list(val) when is_list(val) do
    Enum.map(val, &load_foreign_list/1)
  end

  defp load_foreign_list(val), do: val
end
