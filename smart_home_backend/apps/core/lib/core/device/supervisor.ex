defmodule Core.Device.Supervisor do
  @moduledoc false
  use Supervisor

  alias DB.Data.Device

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    Device.list_all!()
    |> Enum.map(&get_specs(&1))
    # remove empty
    |> Enum.filter(&(&1 != nil))
    |> Supervisor.init(strategy: :one_for_one)
  end

  defp get_specs(%Device{id: id, name: name, ip: ip, port: port, type: type}) do
    mod = module(type)

    if mod.need_process?() do
      %{
        id: String.to_atom(name),
        start: {mod, :start_link, [String.to_charlist(ip), port, [name: String.to_atom(name), device_id: id]]}
      }
    else
      nil
    end
  end

  @spec module(String.t() | atom()) :: module()
  defp module(type) when is_atom(type) do
    module(Atom.to_string(type))
  end

  defp module(type) do
    ("Elixir.Core.Device." <> type)
    |> String.to_existing_atom()
  end
end
