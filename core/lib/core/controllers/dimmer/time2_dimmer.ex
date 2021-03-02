defmodule Core.Controllers.Dimmer.Time2Dimmer do
  @moduledoc false

  use Core.Controllers.DimmerBeh
  import Witchcraft.Functor

  #  alias Core.Controllers.LightController, as: LC
  #  alias Core.Broadcast, as: Channel

  alias DB.{Port}

  @impl true
  def handle_light_change(%{state: s} = dimmer) do
    if !Port.Dimmer.any_light_on?(dimmer) == s do
      set_state(dimmer, state: !s, deep: false)
    else
      :ok
    end
  end

  @impl true
  def set_state(%{state: _state} = dimmer, ops) do
    s = Keyword.get(ops, :state)
    _deep = Keyword.get(ops, :deep, true)

    Port.cast([dimmer], pwm_fill: 10, state: s)
    |> Core.Device.do_r(:set_time_dimmer)
    |> map(&Port.update/1)
  end

  # Privates
end
