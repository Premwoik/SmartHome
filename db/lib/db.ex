defmodule DB do
  @moduledoc """
  Documentation for Db.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Db.hello
      :world

  """
  def start(type \\ :dev) do
    DB.Application.start(type, nil)
  end

  def inc_ref(%{ref: ref}, params \\ %{}, all_str \\ false) do
    name = if all_str, do: "ref", else: :ref
    Map.put(params, name, ref_not_nil(ref) + 1)
  end

  defp ref_not_nil(ref) do
    case ref do
      nil ->
        1

      r ->
        r
    end
  end

  def check_ref(%{ref: ref}, %{ref: ref2}) do
    ref == ref2
  end

  def check_ref(_, _) do
    true
  end
end
