defmodule Device.Supervisor do
  @moduledoc false
  use Supervisor

  alias DB.Dao
  alias DB.Device

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    Dao.get_devices
    |> Enum.map(&(get_specs &1))
    |> Enum.filter(&(&1 != %{})) #remove empty
#    |> add_fixed_childes()
    |> Supervisor.init(strategy: :one_for_one)
  end

#  defp add_fixed_childes(childs) do
#    [Alarm.Actions | childs]
#  end

  defp get_specs(%Device{type: type} = device), do:
    get_specs(device, (get_type_info type))

  defp get_specs(_, :none), do: %{}

  defp get_specs(%Device{type: type, name: name, ip: ip, port: port} = device, {:watcher, module}) do
    %{
      id: (String.to_atom name),
      start: {module, :start_link, [(String.to_charlist ip), port, [], [name: (String.to_atom name)]]}
    }
  end

  defp get_specs(%Device{name: name, ip: ip, port: port}, {:server, module}) do
    %{
      id: (String.to_atom name),
      start: {module, :start_link, [(String.to_charlist ip), port, [], [name: (String.to_atom name)]]}
    }
  end


  defp get_type_info(type) do
    module = String.to_atom "Elixir." <> type
    t = apply module, :get_info, []
    {t, module}
  end

end
