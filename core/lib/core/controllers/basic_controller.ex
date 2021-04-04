defmodule Core.Controllers.BasicController do
  @moduledoc """
    A controller for a Port objects.

    API:
    - turn on
    - turn off
    - toggle
    -

  """

  use Core.Controllers.IOBeh

  alias DB.{Port}
  alias Core.Controllers.Port.{BinaryPort, MonoPort, PwmPort, VirtualPort}
  alias Core.Device.Static.Response
  alias Core.Broadcast, as: Channel
  use Witchcraft

  @callback set_state(list(), ops :: list()) :: any()

  @doc "ports "
  @impl Core.Controllers.IOBeh
  def turn_on(ports, ops) do
    set_state(ports, Keyword.put(ops, :state, true))
  end

  @impl Core.Controllers.IOBeh
  def turn_off(ports, ops) do
    set_state(ports, Keyword.put(ops, :state, false))
  end

  @impl Core.Controllers.IOBeh
  def toggle([%{state: state} | _] = ports, ops) do
    if state, do: turn_off(ports, ops), else: turn_on(ports, ops)
  end

  @impl Core.Controllers.IOBeh
  def set_state([], _), do: %Response{}

  def set_state(ports, ops) do
    propagate? = Keyword.get(ops, :propagate, true)
    get_module(ports)
    |> map(fn {mod, ports} -> mod.set_state(ports, ops) end)
    |> fold()
    |> broadcast(propagate?)
  end

  # Privates

  def broadcast(response, false), do: response
  def broadcast(%Response{ok: oks} = resp, true) do
     Enum.map(oks, fn %{id: id, ref: ref} -> Channel.broadcast_item_change("port", id, ref) end)
     resp
  end

  @spec get_module(list(%Port{}) | String.t()) :: list({module(), list(%Port{})}) | module()
  defp get_module(ports) when is_list(ports) do
    Enum.group_by(ports, fn p -> p.mode end)
    |> Enum.map(fn {k, v} -> {get_module(k), v} end)
  end

  defp get_module(mode) do
    case mode do
      :output -> BinaryPort
      :output_pulse -> MonoPort
      :output_pwm -> PwmPort
      _ -> VirtualPort
    end
  end
end
