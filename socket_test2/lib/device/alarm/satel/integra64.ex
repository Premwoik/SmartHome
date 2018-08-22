defmodule Alarm.Satel.Integra64 do
  @moduledoc false
  use Task
  require Logger

  def get_info, do:
    :watcher

  def start_link(host, port, opts, keywords, timeout \\ 1000) do
    Logger.info("Initializing Alarm.Integra64")
    Task.start_link(__MODULE__, :output_checker, [{host, port}, keywords, timeout])
  end

  def reload, do:
    Process.exit(self(), :normal)

  def output_checker(addr, keywords, timeout, last_read \\ []) do
    receive do
    after
      timeout ->
        case SatelProtocol.iviolation addr do
          {:ok, read} ->
            uread = read -- last_read
            ureadDown = last_read -- read

            dname = keywords[:name]
            proceed_up dname, uread
            proceed_down dname, ureadDown
            log uread, ureadDown

            output_checker addr, keywords, timeout, read
          error ->
            Logger.error("Satel read error: #{inspect error}")
            output_checker addr, keywords, timeout, last_read
        end
    end
  end

  ## Privates
  defp proceed_up(_, []), do: :ok
  defp proceed_up(dname, read), do:
    Alarm.Actions.invoke_up_actions dname, read


  defp proceed_down(_, []), do: :ok
  defp proceed_down(dname, read), do:
    Alarm.Actions.invoke_down_actions dname, read

  defp log(up, down) do
    if ((length up) > 0 || (length down) > 0), do:
      Logger.debug(
        "active outputs - up: #{inspect(up, charlists: :as_lists)}, down: #{inspect(down, charlists: :as_lists)}"
      )
  end

end
