defmodule Core.Controller do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias Core.Controllers.{
    LightController,
    DimmerController,
    SunblindController,
    BasicController,
    ActionController,
    TaskController
  }

  alias DB.{Light, Dimmer, Port, Action, Task, Sunblind}

  @impl IOBeh
  def toggle(items, ops) do
    with {:ok, mod} <- get_module(items) do
      mod.toggle(items, ops)
    end
  end

  @impl IOBeh
  def turn_on(items, ops) do
    with {:ok, mod} <- get_module(items) do
      mod.turn_on(items, ops)
    end
  end

  @impl IOBeh
  def turn_off(items, ops) do
    with {:ok, mod} <- get_module(items) do
      mod.turn_off(items, ops)
    end
  end

  @impl IOBeh
  def read(items, ops) do
    with {:ok, mod} <- get_module(items) do
      mod.read(items, ops)
    end
  end

  # Privates

  defp get_module([item | _] = items) when is_list(items), do: get_module(item)

  defp get_module(item) do
    case item do
      %Light{} -> {:ok, LightController}
      %Dimmer{} -> {:ok, DimmerController}
      %Port{} -> {:ok, BasicController}
      %Sunblind{} -> {:ok, SunblindController}
      %Task{} -> {:ok, TaskController}
      %Action{} -> {:ok, ActionController.Activate}
      _ -> {:error, "Not supported item type!"}
    end
  end
end
