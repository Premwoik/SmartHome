defmodule Core.Controllers.Dimmer.TimeDimmer do
  @moduledoc false

  use Core.Controllers.DimmerController

  alias Core.Controllers.LightController, as: LC
  alias Core.Broadcast, as: Channel

  alias DB.{Dimmer, Port, Repo}
  import Ecto.Changeset, only: [change: 2]

  @impl true
  def get_state(dimmer) do
    {:ok, dimmer.fill > 0}
  end

  @impl true
  def notify_light_change(%{port: %{state: true}} = dimmer) do
    if !Dimmer.any_light_on?(dimmer) do
      set_state(dimmer, false)
    else
      :ok
    end
  end

  def notify_light_change(dimmer) do
    if Dimmer.any_light_on?(dimmer) do
      set_state(dimmer, true)
    else
      :ok
    end
  end

  @impl true
  def set_state(dimmer, s) do
    fill = if(s, do: 100, else: 0)
    set_brightness(dimmer, fill)
  end

  @impl true
  def set_brightness(%{fill: dfill} = dimmer, fill) when dfill != fill do
    dimmer_ = %{dimmer | fill: fill, port: %{dimmer.port | state: fill > 0}}

    cond do
      dfill == 0 ->
        :ok = LC.notify_dimmer_change(dimmer_)
        send_brightness(dimmer, fill)

      fill == 0 ->
        :ok = LC.notify_dimmer_change(dimmer_)
        update_object(dimmer, 0, 1)

      true ->
        send_brightness(dimmer, fill)
    end
  end

  def set_brightness(_, _), do: :ok

  # Privates

  defp send_brightness(dimmer, brightness) do
    {time, dir} = Dimmer.fill_to_time(dimmer, brightness)
    ports = [%Port{DB.Repo.preload(dimmer.port, :device) | timeout: time}]

    with :ok <- Core.Device.do_(:set_time_dimmer, ports) do
      update_object(dimmer, brightness, dir)
    end
  end

  defp update_object(dimmer, fill, dir) do
    {:ok, _} =
      Repo.transaction(fn ->
        Repo.update!(change(dimmer, fill: fill, direction: dir))
        Repo.update!(change(dimmer.port, state: fill > 0))
      end)

    Channel.broadcast_item_change("dimmer", dimmer.id, dimmer.ref + 1)
    :ok
  end
end
