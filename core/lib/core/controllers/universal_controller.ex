defmodule Core.Controllers.UniversalController do
  @moduledoc false

  alias Core.Controllers.{LightController, DimmerController, SunblindController, BasicController, ActionController, TaskController}
  alias DB.{Light, Dimmer, Port, Action, Task, Sunblind}

  def toggle(%Light{port: p} = l) do
    if p.state do
      LightController.turn_off([l])
    else
      LightController.turn_on([l])
    end
  end
  def toggle(%Dimmer{port: p} = d) do
    if d.fill == 0 do
      DimmerController.set_brightness(d, 100)
    else
      DimmerController.set_brightness(d, 0)
    end
  end
  def toggle(%Sunblind{} = s) do
      SunblindController.click(s)
  end
  def toggle(%Port{} = p) do
    if p.state do
      BasicController.turn_off([p])
    else
      BasicController.turn_on([p])
    end
  end
  def toggle(%Action{} = a) do
    if a.isAlive do
      ActionController.turn_off([a])
    else
      ActionController.turn_on([a])
    end
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



end
