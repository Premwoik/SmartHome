defmodule Core.Controllers.SunblindController do
  @moduledoc false


  @behaviour Core.Controllers.Controller

  alias DB.{Port, Sunblind}
  alias Core.Controllers.BasicController
  import Core.Controllers.BasicController, only: [prepare_for_basic: 1, flatten_result: 1]

  def close(sunblinds) do
    sunblinds
    |> Enum.filter(&(&1.state == "open"))
    |> prepare_for_basic
    |> BasicController.turn_on()
    |> proceed_result(sunblinds, "close")
  end

  def open(sunblinds) do
    sunblinds
    |> Enum.filter(&(&1.state == "close"))
    |> prepare_for_basic
    |> BasicController.turn_off()
    |> proceed_result(sunblinds, "open")
  end


  @impl true
  def toggle(sunblinds) do
    for {state, sunblinds_} <- Enum.group_by(sunblinds, &(&1.state)) do
      case state do
        "open" -> open(sunblinds_)
        "close" -> close(sunblinds_)
        _other -> {:error, sunblinds_, "Actual state can't be toggled"}
      end
    end
    |> flatten_result()
  end

  def click(sunblind) do
    case sunblind.state do
      "close" -> open([sunblind])
      "open" -> close([sunblind])
      "position" -> change_position(sunblind)
      "in_move" -> {:error, "still is moving"}
    end
  end

  def calibrate(sunblind, state) do
    Sunblind.update_state(sunblind, state)
  end

  def change_position(sunblind) do
    [sunblind]
    |> prepare_for_basic()
    |> BasicController.impulse()
  end


  #
  #  def set_position(sunblinds, position) do
  #    for s <- sunblinds do
  #      Task.start fn ->
  #
  #      end
  #    end
  #    |> flatten_result()
  #  end




  #  defp proceed_position(sunblind, position) do
  #    abs_pos = abs(s.position - position)
  #    time_to_sleep = sunblind.full_open_time * abs_pos / 100
  #    ports = prepare_for_basic([sunblind])
  #
  #    with sun <- to_position_mode(sunblind),
  #         {:ok, sun_} <- prepare_direction(sun, position),
  #         :ok <- BasicController.turn_on(ports),
  #         :ok <- BasicController.recv_postpone_resp(1000),
  #         :ok <- Process.sleep(time_to_sleep),
  #         :ok <- BasicController.turn_on(ports),
  #         :ok <- BasicController.recv_postpone_resp(1000)
  #      do
  #      %{toggle_direction(sun_) | position: position}
  #      :ok
  #    end
  #
  #  end
  #
  #  defp to_position_mode(sunblind) do
  #    case sunblind.position do
  #      "position" -> sunblind
  #      "open" -> %{sunblind | state: "position", position: 0, direction: "down"}
  #      "close" -> %{sunblind | state: "position", position: 100, direction: "up"}
  #    end
  #  end
  #
  #  defp prepare_direction(sun, pos) do
  #    if (sun.direction == "up") == (sun.position < pos) do
  #      {:ok, sun}
  #    else
  #      revert_direction(sun)
  #    end
  #  end
  #
  #  @spec revert_direction(s :: %DB.Sunblind{}) :: %DB.Sunblind{}
  #  defp revert_direction(s) do
  #    ports = prepare_for_basic [s]
  #
  #    with :ok <- BasicController.turn_on(ports),
  #         :ok <- BasicController.recv_postpone_resp(1000),
  #         :ok <- BasicController.turn_off(ports),
  #         :ok <- BasicController.recv_postpone_resp(1000)
  #      do
  #      toggle_direction(s)
  #    end
  #  end
  #
  #  defp toggle_direction(sunblind) do
  #    case sunblind.direction do
  #      "up" -> {:ok, %{sunblind | direction: "down"}}
  #      "down" -> {:ok, %{sunblind | direction: "up"}}
  #    end
  #  end

  #Privates

  defp proceed_result(res, sunblinds, state) do
    case res do
      :ok -> {sunblinds, :ok}
      {:error, s, error} = res -> {filter_invalid(sunblinds, s), res}
    end
    |> fn {sunblinds_, res_} ->
      Sunblind.update_state(sunblinds_, "in_move")
      Task.start(fn -> unblock_sunblinds(sunblinds_, state) end)
      res_
       end.()
  end

  defp filter_invalid(sunblinds, invalid) do
    Enum.filter(sunblinds, fn s -> !Enum.any?(invalid, fn i -> i.id == s.port.id end) end)
  end

  defp unblock_sunblinds(sunblinds, state) do
    sunblinds
    |> Enum.group_by(fn s -> s.full_open_time end)
    |> Enum.sort()
    |> Enum.each(
         fn {t, s} ->
           receive do
           after t ->
             Sunblind.update_state(s, state)
           end
         end
       )
  end

end
