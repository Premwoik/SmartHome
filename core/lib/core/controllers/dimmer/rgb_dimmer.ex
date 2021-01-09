defmodule Core.Controllers.Dimmer.RgbDimmer do
  @moduledoc false

  use Core.Controllers.DimmerController
  alias DB.{Port, Dimmer}
  alias Core.Broadcast, as: Channel

  @impl true
  def get_state(dimmer) do
    {:ok, dimmer.port.state}
  end

  @impl true
  def set_state(dimmer, s) do
    %{dimmer | port: %{dimmer.port | state: s}}
    |> Core.Device.do_r(:set_state)
    |> process_response(dimmer)
  end

  @impl true
  def set_brightness(dimmer, fill) do
    %{dimmer | fill: fill}
    |> Core.Device.do_r(:set_brightness)
    |> process_response(dimmer)
  end

  @impl true
  def set_color(dimmer, r, g, b) do
    %{dimmer | red: r, green: g, blue: b}
    |> Core.Device.do_r(:set_color)
    |> process_response(dimmer)
  end

  @impl true
  def set_white_brightness(dimmer, fill) do
    %{dimmer | white: fill}
    |> Core.Device.do_r(:set_white_brightness)
    |> process_response(dimmer)
  end

  # Privates

  defp process_response(response, dimmer) do
    with {:ok, dimmer_} <- response do
      Dimmer.changeset(dimmer, Map.from_struct(dimmer_))
      |> DB.Repo.update()

      Port.changeset(dimmer.port, Map.from_struct(dimmer_.port))
      |> DB.Repo.update()

      Channel.broadcast_item_change("dimmer", dimmer.id, dimmer.ref + 1)
      :ok
    end
  end
end
