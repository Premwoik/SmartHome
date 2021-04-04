defmodule Core.Broadcast do
  @moduledoc false

  defmacro __using__(_) do
    quote([]) do
      @behaviour Core.Broadcast
      def broadcast_inputs_change(_, _), do: {:error, "Not implemented yet"}
      def broadcast_outputs_change(_, _), do: {:error, "Not implemented yet"}
      def broadcast_item_change(_, _, _), do: {:error, "Not implemented yet"}

      defoverridable broadcast_inputs_change: 2,
                     broadcast_outputs_change: 2,
                     broadcast_item_change: 3
    end
  end

  @callback broadcast_inputs_change(integer, list(integer)) :: any
  @callback broadcast_outputs_change(integer, list(integer)) :: any
  @callback broadcast_item_change(String.t(), integer, integer) :: any

  @broadcast Application.get_env(:core, :broadcast_handler, Core.Broadcast.BroadcastHandlerMock)

  def broadcast_item_change(type, id, ref), do: @broadcast.broadcast_item_change(type, id, ref)
  def broadcast_item_change(items, type) do
    Enum.each(items, fn %{id: id, ref: ref} ->
      @broadcast.broadcast_item_change(type, id, ref)
    end)
    items
  end

  def broadcast_inputs_change(device_id, up),
    do: @broadcast.broadcast_inputs_change(device_id, up)

  def broadcast_outputs_change(device_id, up),
    do: @broadcast.broadcast_outputs_change(device_id, up)
end
