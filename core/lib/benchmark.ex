defmodule Benchmark do
  @moduledoc false
  def measure(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end

  def measure_r(function) do
    function
    |> :timer.tc
    |> fn {time, res} -> {time/ 1_000_000, res} end.()
  end


end


#(for n <- 1..100, do: Benchmark.measure fn -> Process.sleep(500); :python.call inst, :alarm, :read_outputs, [] end) |> Enum.sum |> (&(&1/100)).()
#(for n <- 1..100, do: Benchmark.measure fn -> Process.sleep(500); SatelProtocol.iviolation {'192.168.2.136', 9000} end) |> Enum.sum |> (&(&1/100)).()
