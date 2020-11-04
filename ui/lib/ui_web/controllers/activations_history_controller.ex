defmodule UiWeb.ActivationsHistoryController do
  use UiWeb, :controller
  alias DB.{DeviceActivations, InputActivations, OutputActivations}


  def get_input_activations(conn, %{"id" => port_id}) do
    data = InputActivations.find(port_id, limit = 50)
           |> Enum.map(&log_mapper/1)
    json(conn, data)
  end

  def get_output_activations(conn, %{"id" => port_id}) do
    data = OutputActivations.find(port_id, limit = 50)
           |> Enum.map(&log_mapper/1)
    json(conn, data)
  end

  def get_device_activations(conn, %{"id" => device_id}) do
    data = DeviceActivations.find(device_id, limit = 50)
           |> Enum.map(&log_mapper/1)
    json(conn, data)
  end

  def get_temperature(conn, %{"id" => therm_id}) do

  end

  defp log_mapper(l) do
    %{
      date: l.date,
      value: l.value,
    }
  end
end
