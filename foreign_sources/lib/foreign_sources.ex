defmodule ForeignSources do
  @moduledoc """
  Documentation for ForeignSources.
  """
  
  @path Application.get_env(:foreign_sources, :path)

  @doc """
  Hello world.


  ## Examples

      iex> ForeignSources.hello()
      :world

  """
 def path(path) do 
   Path.join :code.priv_dir(:foreign_sources), path
 end

  def test_python do
    System.cmd "python3", [path "test.py"]

  end
end
