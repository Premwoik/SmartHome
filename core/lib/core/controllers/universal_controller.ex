defmodule Core.Controllers.UniversalController do
  @moduledoc false

  alias Core.Controllers.{LightController, DimmerController, SunblindController, BasicController, ActionController,
                          TaskController}
  alias DB.{Light, Dimmer, Port, Action, Task, Sunblind}

  #
  @spec toggle(map()) :: any()
  def toggle(item)
  def toggle(%Light{} = l) do
    LightController.toggle([l])
  end
  def toggle(%Dimmer{} = d) do
    DimmerController.toggle([d])
  end
  def toggle(%Sunblind{} = s) do
    SunblindController.click(s)
  end
  def toggle(%Port{} = p) do
    BasicController.toggle([p])
  end
  def toggle(%Action{} = a) do
    Core.Actions.activate_up([a.id])
  end
  def toggle(%Task{} = t) do
    if t.status == "inactive" do
      TaskController.turn_on([t])
    else
      TaskController.turn_off([t])
    end
  end
  def toggle(nil) do
    :ok
  end

  def turn_on(item) do
    :ok
  end

  def turn_off(item) do
    :ok
  end

end
