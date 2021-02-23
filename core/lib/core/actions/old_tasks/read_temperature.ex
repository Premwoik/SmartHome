defmodule Core.Actions.ReadTemperature do
  @moduledoc false

  require Logger
  alias Core.Controllers.ThermometerController, as: Thermometers
  alias DB.{Meter, Action}

  #  @device Application.get_env(:core, :device_helper)
  #  @actions Application.get_env(:core, :actions_server)
  #
  #  @device Core.Device
  #  @actions Core.Actions

  @behaviour Core.Actions.Action

  @impl true
  def execute(_on_off, action, _state) do
    device = Action.arguments(action) |> List.first()

    with {:ok, {addr, raw_temp}} <- Thermometers.read(device) do
      case Meter.identify(device.id, addr) do
        nil ->
          add_new_thermometer(device, addr)

        temp ->
          save_temp_read(temp.id, raw_temp)
      end
    end
  end

  @impl true
  def init_state() do
    :empty
  end

  defp add_new_thermometer(device, addr) do
    Meter.new(name: "NEW_TEMP", address: addr, device_id: device.id, type: :temperature)
    |> Meter.insert()

    Logger.info("Add new thermometer with address #{inspect(addr)}")
  end

  defp save_temp_read(temp_id, raw_temp) do
    tempC = raw_temp * 0.0078125

    %DB.Stats.Temperature.Reading{value: tempC, meter_id: temp_id}
    |> DB.StatsRepo.insert!()

    Logger.info("Read temps #{inspect(tempC)} | thermometer_id #{inspect(temp_id)}")
  end
end
