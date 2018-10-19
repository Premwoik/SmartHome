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

#
#  def set_position(sunblinds, position, check_type \\ true)
#  def set_position(sunblinds, position, true) do
#    for {state, sunblinds_} <- Enum.group_by(sunblinds, &(&1.type)) do
#      case state do
#        "pulse" -> set_position(sunblinds_, position, false)
#        _other -> {:error, sunblinds_, "Wrong sunblind type"}
#      end
#    end
#    |> flatten_result()
#  end
#  def set_position(sunblinds, position, false) do
#    for s <- sunblinds do
#      Task.start fn ->
#
#      end
#    end
#    |> flatten_result()
#  end
#
#
#  defp prepare(s, position) do
#    dir_up? = s_.direction == "up"
#    pos_gt? = s.position < p
#
#    cond do
#      dir_up? && pos_gt? -> :ok
#      dir_up? && !pos_gt? -> :stop_and_go
#      !dir_up? && pos_gt? -> :stop_and_go
#      true -> :ok
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
