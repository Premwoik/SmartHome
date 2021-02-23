defmodule Core.Actions.Heartbeat do
  @moduledoc false
  @behaviour Core.Actions.Action
  #  alias Core.Devices
  require Logger
  alias DB.Action
  alias Core.Device.Static.Response

  @device Core.Device

  @impl true
  def execute(_on_off, action, _) do
    d = Action.arguments(action) |> List.first()

    {time, res} = Benchmark.measure_r(fn -> @device.do_(:heartbeat, d) end)

    case Response.result(res, d.id) do
      {:ok, []} ->
        Logger.debug("Heartbeat to #{inspect(d.name)} passed with time: #{time}sec.")

      err ->
        Logger.error("Heartbeat to #{inspect(d.name)} failed with result #{inspect(err)}.")
    end

    :ok
  end

  @impl true
  def init_state() do
    :empty
  end
end
