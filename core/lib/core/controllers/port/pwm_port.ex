defmodule Core.Controller.Port.PwmPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController

  alias DB.{Port}
  import Core.Controllers.Universal, only: [get_passed_items: 2]

  @impl true
  def set_state(ports, fill: fill) do
    pwm(ports, fill)
  end

  def set_state(_, _), do: {:error, "Fill parameter is missing."}

  #  Privates

  defp pwm(ports, fill) do
    state = if fill > 0, do: true, else: false
    ports = Enum.map(ports, fn p -> %{p | state: state, pwm_fill: fill} end)
    res = Core.Device.do_(:set_pwm_outputs, ports)

    get_passed_items(res, ports)
    |> Port.update(%{state: state, pwm_fill: fill})

    res
  end
end
