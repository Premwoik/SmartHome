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
end
