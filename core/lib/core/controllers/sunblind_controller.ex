defmodule Core.Controllers.SunblindController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel
  alias DB.Port
  alias Core.Controllers.{PortController}
  import Witchcraft.Functor

  @impl IOBeh
  def turn_on(sunblinds, _ops), do: open(sunblinds)
  @impl IOBeh
  def turn_off(sunblinds, _ops), do: close(sunblinds)

  @impl IOBeh
  def toggle([], _ops), do: :ok

  def toggle([s | _] = sunblinds, _ops) do
    case s.more.state do
      :open -> close(sunblinds)
      :close -> open(sunblinds)
    end
  end

  def close(sunblinds) do
    skip_not(sunblinds, :open)
    |> PortController.turn_on()
    |> map(&lock_sunblinds(&1, :close))
  end

  def open(sunblinds) do
    skip_not(sunblinds, :close)
    |> PortController.turn_off()
    |> map(&lock_sunblinds(&1, :open))
  end

  def click(sunblind) do
    case sunblind.state do
      :close -> open([sunblind])
      :open -> close([sunblind])
      :in_move -> {:error, sunblind, "still is moving"}
    end
  end

  def calibrate(sunblind, state) do
    Port.update(sunblind, more: [state: state])
  end

  # Privates

  defp skip_not(sunblinds, state) do
    Enum.filter(sunblinds, &(Port.from_more(&1, :state) == state))
  end

  defp lock_sunblinds(sunblinds, state) do
    sunblinds = update_state(sunblinds, :in_move)
    Task.start(fn -> unlock_sunblinds(sunblinds, state) end)
    sunblinds
  end

  def unlock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> Port.from_more(s, :full_open_time) end)
    |> Enum.sort()
    |> Enum.each(fn {t, ss} ->
      receive do
      after
        t ->
          update_state(ss, state, true)
      end
    end)
  end

  defp update_state(sunblinds, state, broadcast \\ false) do
    Enum.map(sunblinds, fn s ->
      port = Port.update(s, more: [state: state])
      if broadcast do
        Channel.broadcast_item_change(:sunblind, port)
      end
      port
    end)
  end
end
