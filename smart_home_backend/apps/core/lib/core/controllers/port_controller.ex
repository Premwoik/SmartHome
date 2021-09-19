defmodule Core.Controllers.PortController do
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

  alias Core.Device
  alias Core.Device.Static.Response
  alias DB.Proc.PortListProc

  @impl true
  def set_state([], _, _), do: %Response{}

  def set_state(ports, state, _opts) do
    ports = Enum.map(ports, &put_in(&1, [Access.key!(:state), Access.key!("value")], state))

    Device.do_r(ports, :set_outputs)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end

  @impl true
  def set_fill(ports, fill, _opts) do
    Enum.map(ports, &put_in(&1, [:state, "fill"], fill))
    |> Device.do_r(:set_fill)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end
end
