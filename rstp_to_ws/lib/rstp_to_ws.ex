defmodule RstpToWs do
  @moduledoc """
  Documentation for RstpToWs.
  """

  alias RstpToWs.Camera.Converter

  def start(_type, _args) do
    Converter.start_link()
  end
end
