defmodule Core.Controllers.Dimmer.Time2Dimmer do
  @moduledoc false

  use Core.Controllers.DimmerController

  alias Core.Controllers.LightController, as: LC
  alias Core.Broadcast, as: Channel

  alias DB.{Dimmer, Repo}
  import Ecto.Changeset, only: [change: 2]

  @impl true
  def get_state(dimmer) do
    {:ok, dimmer.port.state}
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
  def set_state(%{port: %{state: state} = p} = dimmer, s) when state != s do
#    with :ok <- Core.Controllers.BasicController.toggle([p]) do
    with :ok <- Core.Device.do_(:set_time_dimmer, [p]) do
      :ok = LC.notify_dimmer_change(%{dimmer | port: %{p | state: s}})
      update_object(dimmer, dimmer.fill, dimmer.direction, s)
    end
  end

  def set_state(_, _), do: :ok

  @impl true
  def set_brightness(%{fill: dfill, port: %{state: s}} = dimmer, fill) when dfill != fill and s do
    {time, dir} = Dimmer.fill_to_time2(dimmer, fill)
    async_send(dimmer, fill, dir, time)
  end

  def set_brightness(_, _), do: :ok

  # Privates

  defp async_send(dimmer, fill, dir, [t]) do
    ports = [%{dimmer.port | timeout: t}]

    with :ok <- Core.Device.do_(:set_time_dimmer, ports) do
      update_object(dimmer, fill, dir, dimmer.port.state)
    end
  end

  defp async_send(dimmer, fill, dir, [t1 | _] = ts) do
    ports = [%{dimmer.port | timeout: t1}]

    with :ok <- Core.Device.do_(:set_time_dimmer, ports) do
      Task.start(fn ->
        :timer.sleep(t1)
        async_send(dimmer, fill, dir, ts)
      end)

      :ok
    end
  end

  defp update_object(dimmer, fill, dir, state) do
    {:ok, _} =
      Repo.transaction(fn ->
        IO.inspect(dimmer.port)
        IO.inspect(state)
        Repo.update!(change(dimmer, fill: fill, direction: dir))
        Repo.update!(change(dimmer.port, state: state))
      end)

    Channel.broadcast_item_change("dimmer", dimmer.id, dimmer.ref + 1)
    :ok
  end
end
