defmodule Core.Controllers.SunblindController do
  @moduledoc false

  @behaviour Core.Controllers.Controller

  alias UiWeb.DashboardChannel.Helper, as: Channel
  alias DB.{Port, Sunblind}
  alias Core.Controllers.BasicController
  import Core.Controllers.BasicController, only: [prepare_for_basic: 1, flatten_result: 1]

  def close(sunblinds) do
    valid_sunblinds = skip_not(sunblinds, "open")

    valid_sunblinds
    |> to_ports("close")
    |> BasicController.turn_on()
    |> proceed_result(valid_sunblinds, "close")
  end

  def open(sunblinds) do
    valid_sunblinds = skip_not(sunblinds, "close")

    valid_sunblinds
    |> to_ports("open")
    |> BasicController.turn_off()
    |> proceed_result(valid_sunblinds, "open")
  end

  @impl true
  def toggle([s | _ ] = sunblinds) do
      case s.state do
        "open" -> open(sunblinds)
        "close" -> close(sunblinds)
      end
  end

  def click(sunblind) do
    case sunblind.state do
      "close" -> open([sunblind])
      "open" -> close([sunblind])
      "position" -> change_position(sunblind)
      "in_move" -> {:error, sunblind, "still is moving"}
    end
  end

  def calibrate(sunblind, state) do
    Sunblind.update_state(sunblind, state)
  end

  def change_position(sunblind) do
    sunblind
    |> to_port("position")
    |> BasicController.impulse()
  end

  # Privates
  #

  defp skip_not(sunblinds, state) do
    sunblinds
    |> Enum.filter(&(&1.state == state))
  end

  defp to_ports(sunblinds, next_state \\ "") do
    Enum.map(sunblinds, fn s ->
      DB.Repo.preload(s, [Port.preload(), Port.preload(:open_port)]) |> to_port(next_state)
    end)
  end

  defp to_port(%{type: type, port: p, open_port: op}, next_state) do
    case {type, next_state} do
      {"pulse2", "open"} -> op
      _ -> p
    end
  end

  defp proceed_result(res, sunblinds, state) do
    valid_sunblinds =
      case res do
        :ok -> sunblinds
        {:error, s, _} -> filter_invalid(sunblinds, s)
      end

    update(valid_sunblinds, "in_move")

    Task.start(fn -> unblock_sunblinds(valid_sunblinds, state) end)
    res
  end

  defp filter_invalid(sunblinds, invalid) do
    Enum.filter(sunblinds, fn s -> !Enum.any?(invalid, fn i -> i.id == s.port.id end) end)
  end

  def unblock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> s.full_open_time end)
    |> Enum.sort()
    |> Enum.each(fn {t, ss} ->
      receive do
      after
        t ->
          update(ss, state, 2)
      end
    end)
  end

  defp update(sunblinds, state, v \\ 1) do
    Enum.each(sunblinds, fn %{id: id, ref: ref} = s ->
      Sunblind.update(s, %{state: state})
      Channel.broadcast_change("sunblind", id, ref + v)
    end)
  end
end
