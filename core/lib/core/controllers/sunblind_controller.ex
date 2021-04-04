defmodule Core.Controllers.SunblindController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel
  alias DB.{Port, Repo}
  alias Core.Controllers.{BasicController}
  import Core.Controllers.Universal
  import Witchcraft.Functor

  @impl IOBeh
  def turn_on(sunblinds, _ops), do: open(sunblinds)
  @impl IOBeh
  def turn_off(sunblinds, _ops), do: close(sunblinds)

  @impl IOBeh
  def toggle([], _ops), do: :ok

  def toggle([s | _] = sunblinds, _ops) do
    case s.state do
      :open -> open(sunblinds)
      :close -> close(sunblinds)
    end
  end

  def close(sunblinds) do
    skip_not(sunblinds, :open)
    |> to_ports()
    |> BasicController.turn_on(propagate: false)
    |> map(&lock_sunblinds(&1, :close))
  end

  def open(sunblinds) do
    skip_not(sunblinds, :close)
    |> to_ports()
    |> BasicController.turn_off(propagate: false)
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

  defp to_ports(sunblinds) do
    Enum.map(sunblinds, &to_port/1)
  end

  defp to_port(%{more: %{type: type, state: state}} = p) do
    #    takes current state and assumes that the next action is change to opposite one eg. :close -> :open
    case {type, state} do
      {:pulse2, :close} ->
        Port.from_more(p, :open_port_id) |> Repo.preload()

      _ ->
        p
    end
  end

  defp lock_sunblinds(sunblinds, state) do
    sunblinds = map(sunblinds, &back_to_main/1) |> update_state(:in_move)
    Task.start(fn -> unlock_sunblinds(sunblinds, state) end)
    sunblinds
  end

  def back_to_main(port) do
    case port do
      %{type: :sunblind_helper} ->
        Port.from_more(port, :close_port_id) |> Repo.preload()
      _ -> port
    end
  end

  def unlock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> Port.from_more(s, :full_open_time) end)
    |> Enum.sort()
    |> Enum.each(fn {t, ss} ->
      receive do
      after
        t ->
          update_state(ss, state)
      end
    end)
  end

  defp update_state(sunblinds, state) do
    Enum.map(sunblinds, fn s ->
      port = Port.update(s, more: [state: state])
      Channel.broadcast_item_change("sunblind", port.id, port.ref)
      port
    end)
  end
end
