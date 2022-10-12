defmodule Core.Actions.Heartbeat do
  @moduledoc false

  @behaviour Core.Actions.Action
  # @device Core.Device

  require Logger

  # alias Core.Device.Static.Response
  # alias DB.Data.Action

  @impl true
  def execute(_on_off, _action, _) do
    Logger.error("FIXME")
    # FIXME implement for new version
    # d = Action.get_device(action)

    # {time, res} = Benchmark.measure_r(fn -> @device.do_(:heartbeat, d) end)

    # case Response.result(res, d.id) do
    # {:ok, []} ->
    # Logger.debug("Heartbeat to #{inspect(d.name)} passed with time: #{time}sec.")

    # err ->
    # Logger.error("Heartbeat to #{inspect(d.name)} failed with result #{inspect(err)}.")
    # end
    Logger.debug("Heartbeat not implemented!")

    :ok
  end

  @impl true
  def init_state() do
    :empty
  end
end
