defmodule Core.Controllers.PortController do
  @moduledoc """
    A controller for a Port objects.

    API:
    - turn on
    - turn off
    - toggle
    - read 
    - set_fill
  """

  alias DB.Port
  alias Core.Device
  alias Core.Device.Static.Response
  use Witchcraft

  @callback set_state(list(), ops :: list()) :: any()

  @doc """
  
  """
  @spec turn_on([Port], keyword()) :: %Response{}
  def turn_on(ports, opts \\ []) do
    set_state(ports, true, opts)
  end

  @spec turn_on([Port], keyword()) :: %Response{}
  def turn_off(ports, opts \\ []) do
    set_state(ports, false, opts)
  end

  @spec toggle([Port], keyword()) :: %Response{}
  def toggle([%{state: state} | _] = ports, _opts \\ []) do
    if state, do: turn_off(ports, nil), else: turn_on(ports, nil)
  end

  @spec set_state([Port], boolean(), keyword()) :: %Response{}
  def set_state(ports, state, opts \\ [])

  def set_state([], _, _), do: %Response{}

  def set_state(ports, state, _opts) do
    ports
    |> Enum.map(& Port.cast(&1, state: state))
    |> Device.do_r(:set_outputs)
    |> map(&Port.update/1)
  end

  @spec set_fill([Port], integer(), keyword()) :: %Response{}
  def set_fill(ports, fill, _opts \\ []) do
    ports
    |> Enum.map(& DB.Port.cast(&1, more: [fill: fill]))
    |> Device.do_r(:set_fill)
    |> map(&Port.update/1)
  end

  # Privates
end
