defmodule Core.Device.Supervisor do
  @moduledoc false
  use Supervisor

  alias DB.{Device}

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    Device.all()
    |> Enum.map(&get_specs(&1))
    # remove empty
    |> Enum.filter(&(&1 != nil))
    |> Supervisor.init(strategy: :one_for_one)
  end

  defp get_specs(%Device{name: name, ip: ip, port: port, type: type}) do
    mod = module(type)

    if mod.need_process?() do
      %{
        id: String.to_atom(name),
        start:
          {mod, :start_link, [String.to_charlist(ip), port, [], [name: String.to_atom(name)]]}
      }
    else
      nil
    end
  end

  defp module(type) do
    ("Elixir.Core.Device." <> type)
    |> String.to_existing_atom()
  end
end
