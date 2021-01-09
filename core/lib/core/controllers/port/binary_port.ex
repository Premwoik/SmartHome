defmodule Core.Controllers.Port.BinaryPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController
  import Core.Controllers.Universal, only: [get_passed_items: 2]

  @impl true
  def set_state(ports, state: state) do
    response =
      ports
      |> Enum.map(fn p -> %{p | state: state} end)
      |> Core.Device.do_r(:set_outputs)

    :ok = save_passed(ports, response, state)
    response
  end

  #  Privates

  defp save_passed(ports, response, state) do
    get_passed_items(response, ports)
    |> DB.Port.update(state: state)

    :ok
  end
end
