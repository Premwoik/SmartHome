defmodule UiWeb.ActivationsHistoryController do
  use UiWeb, :controller
  alias DB.Stats.{DeviceActivation, InputActivation, OutputActivation}

  def get_input_activations(conn, %{"id" => port_id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)

    data =
      InputActivation.get(port_id, precision, from: from_date, to: to_date, limit: limit)
      |> Enum.map(&log_mapper/1)

    json(conn, data)
  end

  def get_output_activations(conn, %{"id" => port_id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)

    data =
      OutputActivation.get(port_id, precision, from: from_date, to: to_date, limit: limit)
      |> Enum.map(&log_mapper/1)

    json(conn, data)
  end

  def get_device_activations(conn, %{"id" => device_id} = o) do
    precision = Map.get(o, "precision", "hourly") |> String.to_atom()
    from_date = Map.get(o, "from", nil)
    to_date = Map.get(o, "to", nil)
    limit = Map.get(o, "limit", nil)

    data =
      DeviceActivation.get(device_id, precision, from: from_date, to: to_date, limit: limit)
      |> Enum.map(&log_mapper/1)

    json(conn, data)
  end

  defp log_mapper(l) do
    %{
      date: l.date,
      value: l.value
    }
  end
end
