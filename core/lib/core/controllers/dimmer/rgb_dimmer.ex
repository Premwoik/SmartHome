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
    |> Core.Device.do_r(:set_state)
    |> map(&Port.update/1)
  end

  @impl true
  def set_brightness(dimmer, fill) do
    Port.cast(dimmer, fill: fill)
    |> Core.Device.do_r(:set_brightness)
    |> map(&Port.update/1)
  end

  @impl true
  def set_color(dimmer, red: r, green: g, blue: b) do
    Port.cast(dimmer, more: [red: r, green: g, blue: b])
    |> Core.Device.do_r(:set_color)
    |> map(&Port.update/1)
  end

  @impl true
  def set_white_brightness(dimmer, fill: fill) do
    Port.cast(dimmer, more: [white: fill])
    |> Core.Device.do_r(:set_white_brightness)
    |> map(&Port.update/1)
  end

  # Privates

  #  defp process_response(response) do
  #    with {:ok, dimmer} <- response do
  #      dimmer = Port.update(dimmer)
  #      Channel.broadcast_item_change("dimmer", dimmer.id, dimmer.ref)
  #      :ok
  #    end
  #  end
end
