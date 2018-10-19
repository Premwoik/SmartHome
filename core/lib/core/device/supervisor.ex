defmodule Core.Device.Supervisor do
  @moduledoc false
  use Supervisor

  alias DB.Device

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    Device.all()
    |> Enum.map(&(get_specs &1))
    |> Enum.filter(&(&1 != %{})) #remove empty
    |> Supervisor.init(strategy: :one_for_one)
  end

  defp get_specs(%Device{process: false}), do: %{}

  defp get_specs(%Device{type: type} = device), do:
    get_specs(device, (get_type_info type))


  #  defp get_specs(%Device{type: type, name: name, ip: ip, port: port} = device, {:watcher, module}) do
  #    %{
  #      id: (String.to_atom name),
  #      start: {module, :start_link, [(String.to_charlist ip), port, [], [name: (String.to_atom name)]]}
  #    }
  #  end

  defp get_specs(%Device{name: name, ip: ip, port: port}, module) do
    %{
      id: (String.to_atom name),
      start: {module, :start_link, [(String.to_charlist ip), port, [], [name: (String.to_atom name)]]}
    }
  end


  defp get_type_info(type) do
    module = String.to_atom "Elixir." <> type
  end

end
