defmodule Core.Controllers.SunblindController do
  @moduledoc false

  use Core.Controller

  alias Core.Broadcast, as: Channel
  alias Core.Controllers.PortController
  alias Core.Device.Static.Response

  @impl true
  def turn_on(sunblinds, _ops), do: open(sunblinds)
  @impl true
  def turn_off(sunblinds, _ops), do: close(sunblinds)

  @impl true
  def toggle(sunblinds, _ops) do
    Enum.map(sunblinds, fn s ->
      case s.state["position"] do
        "open" -> close(sunblinds)
        "close" -> open(sunblinds)
        "in_move" -> Response.error({:error, "The sunblind is still moving!"}, [s])
      end
    end)
    |> Response.fold()
  end

  def close(sunblinds) do
    skip_not(sunblinds, "open")
    |> PortController.set_state(true)
    |> Response.map(&lock_sunblinds(&1, "close"))
  end

  def open(sunblinds) do
    skip_not(sunblinds, "close")
    |> PortController.set_state(false)
    |> Response.map(&lock_sunblinds(&1, "open"))
  end

  def click(sunblind) do
    case sunblind.state["position"] do
      "close" -> open([sunblind])
      "open" -> close([sunblind])
      "in_move" -> {:error, sunblind, "The sunblind is still moving!"}
    end
  end

  # Privates

  defp skip_not(sunblinds, position) do
    Enum.filter(sunblinds, &(&1[:state]["position"] == position))
  end

  defp lock_sunblinds(sunblinds, state) do
    sunblinds = update_state(sunblinds, "in_move")
    Task.start(fn -> unlock_sunblinds(sunblinds, state) end)
    sunblinds
  end

  def unlock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> s[:state]["full_open_time"] end)
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
      port = update_in(s[:state]["position"], state)

      if broadcast do
        Channel.broadcast_item_change(:sunblind, port)
      end

      port
    end)
  end
end
