defmodule UiWeb.View.Helper do
  @spec obj_to_view(module :: atom, type :: atom, obj :: any) :: any
  def obj_to_view(module, type, obj) do
    if Ecto.assoc_loaded?(obj) do
      module.render("show.json", Map.put(%{}, type, obj))
    else
      nil
    end
  end

  def objs_to_view(module, type, objs) do
    if Ecto.assoc_loaded?(objs) do
      Enum.map(objs, fn x -> module.render("show.json", Map.put(%{}, type, x)) end)
    else
      nil
    end
  end

  def foreign_view(foreigns) when is_list(foreigns),
    do: Enum.map(foreigns, &foreign_view/1)

  def foreign_view({:foreign, mod, id}) do
    %{"@type": "foreign", module: mod, id: id}
  end

  def foreign_view(f), do: f
end
