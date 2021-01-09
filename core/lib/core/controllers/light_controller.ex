defmodule Core.Controllers.LightController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias DB.{Light}
  alias Core.Controllers.{BasicController, DimmerController}
  alias Core.Broadcast, as: Channel
  import Core.Controllers.Universal, only: [prepare_for_basic: 1, flatten_result: 1]

  @impl IOBeh
  def toggle([], _ops), do: :ok

  def toggle([l | _] = lights, ops) do
    if l.port.state, do: turn_off(lights, ops), else: turn_on(lights, ops)
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
    ports = prepare_for_basic(lights)

    with :ok <- BasicController.set_state(ports, ops) do
      :ok = notify_dimmers(lights, state)
      Light.update(lights)

      Enum.each(lights, fn %{id: id, ref: ref} ->
        Channel.broadcast_item_change("light", id, ref + 1)
      end)

      :ok
    end
  end

  #  @impl IOBeh
  def notify_dimmer_change(%{lights: ls} = dimmer) do
    with true <- Ecto.assoc_loaded?(ls),
         {:ok, state} <- DimmerController.get_state(dimmer) do
      set_state(dimmer.lights, state: state)
    else
      false -> :ok
    end
  end

  # Privates

  defp get_dimmers(lights) do
    lights
    |> DB.Repo.preload(dimmer: [:port])
    |> Enum.map(fn l -> l.dimmer end)
    |> Enum.filter(fn d -> d != nil end)
    |> Enum.uniq()
  end

  defp notify_dimmers(lights, state) do
    get_dimmers(lights)
    |> Enum.map(&DimmerController.notify_light_change(&1, state))
    |> flatten_result()
  end
end
