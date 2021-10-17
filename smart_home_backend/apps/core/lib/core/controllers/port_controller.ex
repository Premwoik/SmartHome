defmodule Core.PortController do
  @moduledoc """
    An IOController impelemtation to deliver a basic operations on Port objects.

    Supported callbacks: 
  - `set_state/3`
  - `turn_on/2`
  - `turn_off/2`
  - `toggle/2`
  - `set_fill/3`
  TODO add support for
  - `read`
  """

  use Core.Controller

  alias Core.Broadcast, as: Channel
  alias Core.Device
  alias Core.Device.Static.Response
  alias DB.Data.Port
  alias DB.Proc.PortListProc

  @impl true
  def set_state([], _, _), do: %Response{}

  def set_state(ports, state, opts) do
    ports = Enum.map(ports, &Port.put_state(&1, "value", state))

    Device.do_r(ports, :set_outputs)
    |> Response.map(fn ports ->
      Enum.map(ports, fn p ->
        {:ok, updated_port} = PortListProc.update(p.id, Map.from_struct(p))

        if Keyword.get(opts, :broadcast, true) do
          :ok = Channel.broadcast_item_change(updated_port.type, updated_port)
        end

        updated_port
      end)
    end)
  end

  @impl true
  def set_fill(ports, fill, _opts) do
    Enum.map(ports, &Port.put_state(&1, "fill", fill))
    |> Device.do_r(:set_fill)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end
end
