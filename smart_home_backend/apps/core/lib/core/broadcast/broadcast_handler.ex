defmodule Core.Broadcast.BroadcastHandler do
  @moduledoc false

  use Core.Broadcast

  def broadcast_item_change(_, _), do: :ok
  def broadcast_inputs_change(_, _), do: :ok
  def broadcast_outputs_change(_, _), do: :ok
end
