defmodule Core.DimmerController do
  @moduledoc """
    An IOController implementation to deliver basic operations on dimmers. In addition this module handles some unique operations for the dimmer type.

    Supported operations: 
  - `set_brightness/2`
  - `set_white_brightness/2`
  - `set_color/4`
  - `set_state/3`
  - `turn_on/2`
  - `turn_off/2`
  - `toggle/2`
  - `set_fill/3` - same as `set_brightness/2`
  TODO add support for
  - `read`

  """

  use Core.Controller

  alias Core.Device.Static.Response
  alias Core.PortController
  alias DB.Data.Port
  alias DB.Proc.PortListProc

  @impl true
  def set_state(dimmers, state, ops) do
    PortController.set_state(dimmers, state, ops)
  end

  @spec set_color([Port.t()], 0..255, 0..255, 0..255) :: Response.t()
  def set_color(dimmers, red, green, blue) do
    color = %{red: red, green: green, blue: blue}

    Enum.map(dimmers, &Port.put_state(&1, "color", color))
    |> Core.Device.do_r(:set_color)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end

  @spec set_white_brightness([Port.t()], 0..255) :: Response.t()
  def set_white_brightness(dimmers, fill) do
    Enum.map(dimmers, &Port.put_state(&1, "white", fill))
    |> Core.Device.do_r(:set_white_brightness)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end

  @spec set_brightness([Port.t()], 0..100) :: Response.t()
  def set_brightness(dimmers, fill) do
    Enum.map(dimmers, &Port.put_state(&1, "fill", fill))
    |> Core.Device.do_r(:set_brightness)
    |> Response.map(fn p ->
      {:ok, updated_port} = PortListProc.update(p.id, p)
      updated_port
    end)
  end

  # def handle_light_change(dimmer, _s \\ nil) do
  # with {:ok, mod} <- get_module(dimmer) do
  # mod.handle_light_change(dimmer)
  # end
  # end

  # Privates
  # def update_lights(%{ok: [dimmer], result: [{_, {:ok, [dimmer_res]}}]} = response) do
  # lights_res = Map.fetch!(dimmer_res, "lights") |> Enum.sort_by(& &1["pin"])

  # lights =
  # Port.from_more(dimmer, :lights)
  # |> Enum.sort_by(& &1.number)
  # |> Enum.zip(lights_res)
  # |> Enum.map(fn {l, %{"state" => state}} ->
  # state = if(state == 1, do: true, else: false)
  # light = Port.update(l, state: state)
  # Broadcast.broadcast_item_change(:light, light)
  # light
  # end)

  # dimmer = Port.cast(dimmer, more: [lights: lights])
  # Map.put(response, :ok, [dimmer])
  # end
end
