defmodule Core.Tasks.ReadTemperature do
  @moduledoc false
  @behaviour Core.Tasks.Task
  alias Core.Controllers.ThermometerController, as: Thermometers
  require Logger
#  @device Application.get_env(:core, :device_helper)
#  @actions Application.get_env(:core, :actions_server)
#
  @device Core.Device
  @actions Core.Actions

  @impl true
  def execute(task, _) do
    Thermometers.read(task.device)
    |> case do
      {:ok, {addr, raw_temp}} ->

        case DB.Repo.get_by DB.Thermometer, address: addr do
          nil ->
            add_new_thermometer(task.device, addr)
          temp ->
            save_temp_read(temp.id, raw_temp)
        end
      _ ->
        :error
    end
  end

  @impl true
  def init_state() do
    :empty
  end

  defp add_new_thermometer(device, addr) do
    %DB.Thermometer{name: "NEW_TEMP", address: addr, device_id: device.id}
    |> DB.Repo.insert!
    Logger.info("Add new thermometer with address #{inspect addr}")
  end

  defp save_temp_read(temp_id, raw_temp) do
    tempC = raw_temp * 0.0078125

    %DB.Thermometer.Read{value: tempC, therm_id: temp_id}
    |> DB.Repo.insert!

    Logger.info("Read temps #{inspect tempC} | thermometer_id #{inspect temp_id}")
  end


end
