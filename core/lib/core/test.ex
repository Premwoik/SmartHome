defmodule Core.Test do
  @moduledoc false

  def pw_sunblind() do
    s1 = DB.Repo.get(DB.Sunblind, 2) |> DB.Repo.preload(port: :device)
    s2 = DB.Repo.get(DB.Sunblind, 3) |> DB.Repo.preload(port: :device)
    [s1, s2]
  end

  def parter_sunblind() do
    DB.Repo.get(DB.Sunblind, 1) |> DB.Repo.preload(port: :device, open_port: :device)
  end

  def all_sunblind() do
    DB.Repo.all(DB.Sunblind) |> DB.Repo.preload(port: :device)
  end


end
