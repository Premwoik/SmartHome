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

  def clear_data() do
    DB.InitTest.delete_all()
  end

  def init_test() do
    DB.InitTest.insert_ard_mega(true)
    DB.InitTest.insert_integra64(true)
    DB.InitTest.insert_mock_device(true)
    DB.InitTest.insert_tasks(true)
  end





end
