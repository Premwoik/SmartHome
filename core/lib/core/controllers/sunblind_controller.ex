defmodule Core.Controllers.SunblindController do
  @moduledoc false

  @behaviour Core.Controllers.Controller

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
  def toggle(sunblinds) do
    for {state, sunblinds_} <- Enum.group_by(sunblinds, & &1.state) do
      case state do
        "open" -> open(sunblinds_)
        "close" -> close(sunblinds_)
        _ -> {:error, sunblinds_, "Actual state can't be toggled"}
      end
    end
    |> flatten_result()
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
    |> Enum.filter(&(&1.state ==  state))
  end

  defp to_ports(sunblinds, next_state \\ "") do
    Enum.map(sunblinds, fn s -> DB.Repo.preload(s, [Port.preload, Port.preload(:open_port)]) |> to_port(next_state) end)
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

    Sunblind.update_state(valid_sunblinds, "in_move")
    Task.start(fn -> unblock_sunblinds(valid_sunblinds, state) end)
    res
  end

  defp filter_invalid(sunblinds, invalid) do
    Enum.filter(sunblinds, fn s -> !Enum.any?(invalid, fn i -> i.id == s.port.id end) end)
  end

  defp unblock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> s.full_open_time end)
    |> Enum.sort()
    |> Enum.each(fn {t, s} ->
      receive do
      after
        t ->
          Sunblind.update_state(s, state)
      end
    end)
  end
end
