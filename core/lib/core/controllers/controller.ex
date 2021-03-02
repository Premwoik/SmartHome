defmodule Core.Controller do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh
  alias Witchcraft.Foldable, as: F

  alias Core.Controllers.{
    LightController,
    DimmerController,
    SunblindController,
    BasicController,
    ActionController
  }

  alias Core.Device.Static.Response

  alias DB.{Port, Action}

  @impl IOBeh
  def toggle([], _), do: %Response{}

  def toggle(items, ops) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :toggle, ops))
    |> F.fold()

    #    with {:ok, mod} <- get_module(items) do
    #      mod.toggle(items, ops)
    #    end
  end

  @impl IOBeh
  def turn_on([], _), do: %Response{}

  def turn_on(items, ops) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :turn_on, ops))
    |> F.fold()

    #    with {:ok, mod} <- get_module(items) do
    #      mod.turn_on(items, ops)
    #    end
  end

  @impl IOBeh
  def turn_off([], _), do: %Response{}

  def turn_off(items, ops) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :turn_off, ops))
    |> F.fold()

    #    with {:ok, mod} <- get_module(items) do
    #      mod.turn_off(items, ops)
    #    end
  end

  @impl IOBeh
  def read([], _), do: %Response{}

  def read(items, ops) do
    group_by_controller(items)
    |> Enum.map(&run_controller(&1, :read, ops))
    |> F.fold()

    #    with {:ok, mod} <- get_module(items) do
    #      mod.read(items, ops)
    #    end
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
    case to_string(t) do
      "sunblind" -> {:ok, SunblindController}
      "light" -> {:ok, LightController}
      "dimmer" <> _ -> {:ok, DimmerController}
      _ -> {:ok, BasicController}
    end
  end
end
