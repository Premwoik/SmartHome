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
  import Core.Controllers.Universal

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
  def set_state(ports, ops) do
    get_module(ports)
    |> Enum.map(fn {mod, ports} -> mod.set_state(ports, ops) end)
    |> flatten_result()
  end

  # Privates

  @spec get_module(list(%Port{}) | String.t()) :: list({module(), list(%Port{})}) | module()
  defp get_module(ports) when is_list(ports) do
    Enum.group_by(ports, fn p -> p.mode end)
    |> Enum.map(fn {k, v} -> {get_module(k), v} end)
  end

  defp get_module(mode) do
    case mode do
      "output" -> BinaryPort
      "output-pulse" -> MonoPort
      "output-pwm" -> PwmPort
      _ -> VirtualPort
    end
  end
end
