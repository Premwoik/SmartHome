defmodule Core.Broadcast do
  @moduledoc false

  defmacro __using__(_) do
    quote([]) do
      @behaviour Core.Broadcast
      def broadcast_inputs_change(_, _), do: {:error, "Not implemented yet"}
      def broadcast_outputs_change(_, _), do: {:error, "Not implemented yet"}
      def broadcast_item_change(_, _), do: {:error, "Not implemented yet"}

      defoverridable broadcast_inputs_change: 2,
                     broadcast_outputs_change: 2,
                     broadcast_item_change: 2
    end
  end

  @callback broadcast_inputs_change(integer, list(integer)) :: any
  @callback broadcast_outputs_change(integer, list(integer)) :: any
  @callback broadcast_item_change(String.t(), struct()) :: any

  def broadcast_item_change(type, item) do
    Core.HandyConfig.get_broadcast_handlers()
    |> Enum.each(& &1.broadcast_item_change(type, item))
  end

  def broadcast_inputs_change(device_id, up) do
    Core.HandyConfig.get_broadcast_handlers()
    |> Enum.each(& &1.broadcast_inputs_change(device_id, up))
  end

  def broadcast_outputs_change(device_id, up) do
    Core.HandyConfig.get_broadcast_handlers()
    |> Enum.each(& &1.broadcast_outputs_change(device_id, up))
  end
end