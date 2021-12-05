defmodule Core.Actions.MonitorHeating do
  @moduledoc false

  @behaviour Core.Actions.Action

  require Logger

  alias DB.Data.Device
  alias DB.Proc.PortListProc

  @impl true
  def execute(_on_off, action, _state) do
    device_id = action.attributes["device_id"]

    with {:ok, device} <- Device.find(device_id),
         {:ok, %{circuts: [high, low]}} <- Core.Device.do_(:get_config, device),
         {:ok, ports} <- PortListProc.identify(device_id, [0, 1]) do
      Enum.zip([low, high], ports)
      |> Enum.each(fn {%{status: status, current_temp: temp}, port} ->
        value = status == :running

        PortListProc.update_state(port.id, %{
          "value" => value,
          "status" => to_string(status),
          "temp" => temp
        })
      end)
    end

    :ok
  end

  @impl true
  def init_state() do
    %{}
  end
end
