defmodule Core.Controllers.LightController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias DB.{Port, Repo}
  alias Core.Controllers.{BasicController, DimmerController}
  alias Core.Broadcast, as: Channel
  import Core.Controllers.Universal, only: [flatten_result: 1]
  alias Core.Device.Static.Response

  @impl IOBeh
  def toggle([], _ops), do: :ok

  def toggle([l | _] = lights, ops) do
    if l.state, do: turn_off(lights, ops), else: turn_on(lights, ops)
  end

  @impl IOBeh
  def turn_on(lights, ops) do
    set_state(lights, Keyword.put(ops, :state, true))
  end

  @impl IOBeh
  def turn_off(lights, ops) do
    set_state(lights, Keyword.put(ops, :state, false))
  end

  @impl IOBeh
  def set_state(lights, ops) do
    state = Keyword.get(ops, :state, nil)
    deep_set = Keyword.get(ops, :deep, true)
    ops = Keyword.put(ops, :propagate, false)

    with %Response{error: [], ok: lights} = resp <- BasicController.set_state(lights, ops) do
      notify_dimmers(lights, state, deep_set)
      broadcast(lights)
      resp
    end
  end

  #  @impl IOBeh
  def handle_dimmer_change(%{state: d_state} = dimmer) do
    Port.Dimmer.lights(dimmer)
    |> Enum.filter(&(&1.state != d_state))
    |> set_state(state: d_state, deep: false)
  end

  # Privates

  defp broadcast(lights) do
    Enum.each(lights, fn %{id: id, ref: ref} ->
      Channel.broadcast_item_change("light", id, ref + 1)
    end)
  end

  defp notify_dimmers(_, _, false) do
    :ok
  end

  defp notify_dimmers(lights, state, true) do
    case get_dimmers(lights) do
      [] ->
        :ok

      dimmers ->
        Enum.map(dimmers, &DimmerController.handle_light_change(&1, state))
        |> Witchcraft.Foldable.fold()
    end
  end

  defp get_dimmers(lights) do
    Enum.map(lights, fn l -> Port.from_more(l, :dimmer_id) end)
    |> Enum.filter(fn d -> d != nil end)
    |> Enum.uniq()
    |> Repo.preload()
  end
end
