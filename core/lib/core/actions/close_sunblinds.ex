defmodule Core.Actions.CloseSunblinds do
  @moduledoc false

  @behaviour Core.Actions.Action

  @impl true
  def execute(on_off, _action, amem) do
    Logger.info("CloseSunblinds action is invoked.")
    state = case on_off,
                 do: (
                   :on -> true;
                   :off -> false)
    data = Dao.get_ports_by_type("sunblind")
           |> Enum.group_by(&(&1.device_id))
           |> Map.to_list()
    for {deviceId, sunblinds} <- data, do:
      Controller.set_outputs (Dao.get_device_atom deviceId), sunblinds, state
    amem
  end

  @impl true
  def init_memory() do
    %{}
  end

end
