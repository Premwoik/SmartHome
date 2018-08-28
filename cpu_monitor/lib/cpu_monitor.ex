defmodule CpuMonitor do

  require Logger
  @moduledoc """
  Documentation for CpuMonitor.
  """

  @doc false
  @spec start_usage_logger(integer, atom) :: {atom, atom}
  def start_usage_logger(frequency \\ 10_000, name \\ __MODULE__) do
    case Process.whereis name do
      nil ->
        {:ok, pid} = Task.start fn -> logging_loop frequency end
        Process.register pid, name
        {:ok, :started}
      _ ->
        {:error, :running}
    end

  end

  @doc false
  @spec stop_usage_logger(atom) :: {atom, atom} | atom
  def stop_usage_logger(name \\ __MODULE__) do
    case Process.whereis name do
      nil ->
        {:error, :not_running}
      pid ->
        send pid, {:stop, self()}
        receive do
          r -> r
          after
            1000 -> :error
        end
    end
  end


  @doc false
  @spec usage() :: integer
  def usage do
    read_cpu_stat()
    |> (calculate_average_idle_percentage)
    |> (idle_to_load)
    |> (round)
  end

  @doc false
  @spec short_usage() :: integer
  def short_usage(time \\ 100) do
    (Task.async fn ->
      r1 = read_cpu_stat()
      Process.sleep(time)
      r2 = read_cpu_stat()
      (Stream.zip r1, r2)
        |> (Enum.map fn {x, y} -> x-y end)
        |> (calculate_average_idle_percentage)
        |> (idle_to_load)
        |> (round)
    end)
    |> (Task.await time+50)
  end

#  Privates

  @spec logging_loop(integer) :: any
  defp logging_loop(frequency) do
    receive do
      {:stop, pid} -> send pid, {:ok, :stopped}
    after
      frequency ->
        Logger.info "CpuMonitor - cpu usage is #{format_usage short_usage()}%"
        logging_loop frequency
    end
  end

  @spec format_usage(integer) :: String.t()
  defp format_usage(usage) do
    if usage < 10 do
      "0" <> Integer.to_string usage
    else
      Integer.to_string usage
    end
  end

  @spec idle_to_load(integer) :: integer
  defp idle_to_load(idle) do
    100 - idle
  end

  @spec calculate_average_idle_percentage(list) :: float
  defp calculate_average_idle_percentage([_, _, _, idle | _] = stats) do
    (idle * 100) / (Enum.sum stats)
  end

  @spec read_cpu_stat() :: list(integer)
  defp read_cpu_stat do
    (System.cmd "cat", ["/proc/stat"])
    |> (unpack)
    |> (String.split "\n")
    |> (List.first)
    |> (String.split)
    |> fn
         ["cpu", user, nice, system, idle, iowait, irq, softirq | _] ->
           [user, nice, system, idle, iowait, irq, softirq]
       end.()
    |> (Enum.map &(String.to_integer &1))
  end

  @spec unpack({String.t(), integer}) :: String.t()
  defp unpack({read, _}), do: read

end
