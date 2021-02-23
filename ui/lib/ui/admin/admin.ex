defmodule Ui.Admin do
  @moduledoc false

  def check_type(%{type: t} = item, type) do
    if String.contains?(to_string(t), to_string(type)) do
      item
    end
  end

  def split_more_fields(params, mod, more_mod) do
    try do
      %{attributes: attrs} = mod.__info__
      %{attributes: more_attrs} = more_mod.__info__
#      TODO add checking more attributes in DB and here unwrap more attribs
      {params, more} =
        Enum.reduce(params, {%{}, %{}}, fn {key, val}, {ready, more} ->
          key = String.to_atom(key)
          if(key in attrs,
            do: {Map.put(ready, key, val), more},
            else: {ready, update_more(more_attrs, more, key, val)}
          )
        end)

      Map.put(params, :more, more)
    rescue
      e -> e
    end
  end

  defp update_more(more_attrs, more, k, v) do
    if(k in more_attrs, do: Map.put(more, k, v), else: more)
  end
end
