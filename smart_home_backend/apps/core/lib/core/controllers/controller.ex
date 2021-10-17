defmodule Core.Controller do
  @moduledoc false

  alias Core.ActionController
  alias Core.DimmerController
  alias Core.PortController
  alias Core.SunblindController
  alias Core.Device.Static.Response
  alias DB.Data.{Port, Action}

  @type item :: Action.t() | Port.t()
  @type state_t :: String.t() | atom() | boolean()
  @type fill_t :: 0..100

  @doc """
  Changes the object state to the opposite.

  - `items` : The list of objects to toggle state. 
  """
  @callback toggle(items :: [item()], ops :: keyword()) :: Response.t()

  @doc """
  Changes the object state to on.

  - `items` : The list of objects to toggle state. 
  """
  @callback turn_on(items :: [item()], ops :: keyword()) :: Response.t()

  @doc """
  Changes the object state to off.

  - `items` : The list of objects to turn on. 
  """
  @callback turn_off(items :: [item()], ops :: keyword()) :: Response.t()

  @doc """
  Changes the object state to the given one.

  - `items` : The list of object to turn off. 
  - `state` : The state to be set. Its type can differ in the implementing modules.
  """
  @callback set_state(items :: [item()], state :: state_t(), ops :: keyword()) :: Response.t()

  @doc """
  Sets the fill value.

  - `items` : The list of objects to set fill. 
  - `fill` : The fill value in range 0-100.
  """
  @callback set_fill(items :: [item()], fill :: fill_t(), ops :: keyword()) :: Response.t()

  @doc """
  Reads the state of object.

  - `items` : The list of object to read.
  """
  @callback read(items :: [item()], ops :: keyword()) :: Response.t()

  def toggle(items, opts \\ []) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :toggle, opts))
    |> Response.fold()
  end

  @spec turn_on(list(), keyword()) :: Response.t()
  def turn_on(items, ops \\ []) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :turn_on, ops))
    |> Response.fold()
  end

  def turn_off(items, ops \\ []) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :turn_off, ops))
    |> Response.fold()
  end

  def set_fill(items, ops \\ []) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :fill, ops))
    |> Response.fold()
  end

  def read(items, ops \\ []) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :read, ops))
    |> Response.fold()
  end

  # Privates
  defp group_by_controller(items) do
    Enum.group_by(items, &get_module/1)
  end

  defp run_controller({{:ok, mod}, items}, fun, opts) do
    Kernel.apply(mod, fun, [items, opts])
  end

  defp run_controller(error, _, _), do: error

  defp get_module([item | _] = items) when is_list(items), do: get_module(item)

  defp get_module(item) do
    case item do
      %Port{} -> port_based_type(item)
      %Action{} -> {:ok, ActionController.Activate}
      _ -> {:error, "Not supported item type!"}
    end
  end

  defp port_based_type(%{type: t}) do
    case t do
      :sunblind -> {:ok, SunblindController}
      :dimmer -> {:ok, DimmerController}
      _ -> {:ok, PortController}
    end
  end

  defmacro __using__(_) do
    quote([]) do
      @behaviour Core.Controller

      @impl true
      def toggle(items, ops \\ [])

      def toggle([i | _] = items, opts) do
        state = i.state["value"]
        set_state(items, !state, opts)
      end

      @impl true
      def turn_on(items, ops \\ [])
      def turn_on(items, ops), do: set_state(items, true, ops)

      @impl true
      def turn_off(items, ops \\ [])
      def turn_off(items, ops), do: set_state(items, false, ops)

      @impl true
      def set_state(items, state, ops \\ [])
      def set_state(items, _, _), do: Response.error({:error, "Not supported"}, items)

      @impl true
      def set_fill(items, fill, ops \\ [])
      def set_fill(items, _, _), do: Response.error({:error, "Not supported"}, items)

      @impl true
      def read(items, ops \\ [])
      def read(items, _), do: Response.error({:error, "Not supported"}, items)

      defoverridable turn_on: 2, turn_off: 2, toggle: 2, read: 2, set_state: 3, set_fill: 3
    end
  end
end
