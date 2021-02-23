defmodule Core.Controller.Port.PwmPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController

  alias DB.{Port}
  alias Core.Device.Static.Response
  use Witchcraft

  @impl true
  def set_state(ports, fill: fill) do
    state = if fill > 0, do: true, else: false

    Port.cast(ports, state: state, pwm_fill: fill)
    |> Core.Device.do_r(:set_pwm_outputs)
    |> map(&Port.update/1)
  end

  def set_state(ports, _), do: Response.error({:error, "Fill parameter is missing."}, ports)

  #  Privates
end
