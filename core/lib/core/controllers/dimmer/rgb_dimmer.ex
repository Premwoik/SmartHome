defmodule Core.Controllers.Dimmer.RgbDimmer do
  @moduledoc false

  use Core.Controllers.DimmerBeh
  alias DB.{Port}
  #  alias Core.Broadcast, as: Channel
  #  use Witchcraft
  import Witchcraft.Functor

  @impl true
  def set_state(dimmer, state: s) do
    Port.cast(dimmer, state: s)
    |> List.wrap()
    |> Core.Device.do_r(:set_state)
    |> map(&Port.update/1)
  end

  @impl true
  def set_brightness(dimmer, fill: fill) do
    Port.cast(dimmer, fill: fill)
    |> List.wrap()
    |> Core.Device.do_r(:set_brightness)
    |> map(&Port.update/1)
  end

  @impl true
  def set_color(dimmer, red: r, green: g, blue: b) do
    Port.cast(dimmer, more: [red: r, green: g, blue: b])
    |> List.wrap()
    |> Core.Device.do_r(:set_color)
    |> map(&Port.update/1)
  end

  @impl true
  def set_white_brightness(dimmer, fill: fill) do
    Port.cast(dimmer, more: [white: fill])
    |> List.wrap()
    |> Core.Device.do_r(:set_white_brightness)
    |> map(&Port.update/1)
  end

  # Privates

end
