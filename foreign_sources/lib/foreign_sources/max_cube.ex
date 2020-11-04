defmodule ForeignSources.MaxCube do
  @moduledoc """

  """
  

  def set_mode_boost(%{rf_address: rf_addr, room_id: id}) do
    {data, 0} = run(["set_mode_boost", to_string(id), rf_addr])
    data == "true\n"
  end

  def set_mode_auto(%{rf_address: rf_addr, room_id: id}) do
    {data, 0} = run(["set_mode_auto", to_string(id), rf_addr])
    data == "true\n"
  end

  def set_mode_manual(%{rf_address: rf_addr, room_id: id}, temp) do
    {data, 0} = run(["set_mode_manual", to_string(id), rf_addr, to_string(temp)])
    data == "true\n"
  end

  def read() do
    {data, 0} = run("read")
  end

  defp run(args) do
    System.cmd("python3", [path("max.py"), "192.168.2.139"] ++ args)
  end

  defp path(path) do
    Path.join(:code.priv_dir(:foreign_sources), path)
  end
end
