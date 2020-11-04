defmodule Core.Actions.CloseSunblinds do
  @moduledoc false
  require Logger
  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController

  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{}
  end

  @impl true
  def execute(:up, action, amem) do
    Benchmark.measure_p(fn ->
      DB.Action.get_args_ids(action)
      |> DB.Sunblind.get_by_port()
      |> SunblindController.close()
    end)

    # Sunblind.get_type("only_close")
    # |> SunblindController.close()
    amem
  end

  def execute(:down, action, amem) do
    Benchmark.measure_p(fn ->
      DB.Action.get_args_ids(action)
      |> DB.Sunblind.get_by_port()
      |> SunblindController.open()
    end)

    # Sunblind.get_type("only_close")
    # |> SunblindController.open()
    amem
  end
end
