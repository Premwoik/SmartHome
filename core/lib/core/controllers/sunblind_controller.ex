defmodule Core.Controllers.SunblindController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Broadcast, as: Channel
  alias DB.{Port, Sunblind}
  alias Core.Controllers.{BasicController}
  import Core.Controllers.Universal

  @impl IOBeh
  def turn_on(sunblinds, _ops), do: open(sunblinds)
  @impl IOBeh
  def turn_off(sunblinds, _ops), do: close(sunblinds)

  @impl IOBeh
  def toggle([], _ops), do: :ok

  def toggle([s | _] = sunblinds, _ops) do
    case s.state do
      "close" -> open(sunblinds)
      "open" -> close(sunblinds)
    end
  end

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

  def click(sunblind) do
    case sunblind.state do
      "close" -> open([sunblind])
      "open" -> close([sunblind])
      "in_move" -> {:error, sunblind, "still is moving"}
    end
  end

  def calibrate(sunblind, state) do
    Sunblind.update_state(sunblind, state)
  end

  # Privates

  defp skip_not(sunblinds, state) do
    Enum.filter(sunblinds, &(&1.state == state))
  end

  defp to_ports(sunblinds, next_state) do
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
    valid_sunblinds = get_passed_items(res, sunblinds)
    update_state(valid_sunblinds, "in_move")
    Task.start(fn -> unblock_sunblinds(valid_sunblinds, state) end)
    res
  end

  def unblock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> s.full_open_time end)
    |> Enum.sort()
    |> Enum.each(fn {t, ss} ->
      receive do
      after
        t ->
          update_state(ss, state, 2)
      end
    end)
  end

  defp update_state(sunblinds, state, v \\ 1) do
    Enum.each(sunblinds, fn %{id: id, ref: ref} = s ->
      Sunblind.update(s, %{state: state})
      Channel.broadcast_item_change("sunblind", id, ref + v)
    end)
  end
end
