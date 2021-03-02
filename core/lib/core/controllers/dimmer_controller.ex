defmodule Core.Controllers.DimmerController do
  @moduledoc false

  import Witchcraft.Functor
  import Witchcraft.Foldable

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  @impl IOBeh
  def toggle(dimmers, _ops) when is_list(dimmers),
    do:
      map(dimmers, &toggle/1)
      |> fold()

  def toggle(dimmer, _ops) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_state(dimmer, !dimmer.state)
    end
  end

  @impl IOBeh
  def turn_on(dimmers, _ops) when is_list(dimmers),
    do:
      map(dimmers, &turn_on/1)
      |> fold()

  def turn_on(dimmer, _ops) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_state(dimmer, state: true)
    end
  end

  @impl IOBeh
  def turn_off(dimmers, _ops) when is_list(dimmers),
    do:
      map(dimmers, &turn_off/1)
      |> fold()

  def turn_off(dimmer, _ops) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_state(dimmer, state: false)
    end
  end

  def set_color(dimmer, red, green, blue) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_color(dimmer, red: red, green: green, blue: blue)
    end
  end

  def set_white_brightness(dimmer, fill) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_white_brightness(dimmer, fill: fill)
    end
  end

  def set_brightness(dimmer, fill) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_brightness(dimmer, fill: fill)
    end
  end

  def handle_light_change(dimmer, _s \\ nil) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.handle_light_change(dimmer)
    end
  end

  # Privates

  def get_module(%{type: t, mode: m} = d) do
    case to_string(t) do
      "dimmer2" ->
        {:ok, Core.Controllers.Dimmer.Time2Dimmer}

      "dimmer" ->
        case to_string(m) do
          "output" ->
            {:ok, Core.Controllers.Dimmer.TimeDimmer}

          "output_pwm" ->
            {:ok, Core.Controllers.Dimmer.PwmDimmer}

          _ ->
            {:error, "Wrong mode"}
        end

      "dimmer_rgb" <> _ ->
        {:ok, Core.Controllers.Dimmer.RgbDimmer}

      _ ->
        {:error, "Wrong type"}
    end
  end
end
