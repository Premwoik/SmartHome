defmodule Core.Controllers.Port.MonoPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController

  alias Core.Controllers.Port.BinaryPort, as: BP
  import Core.Controllers.Universal, only: [get_passed_items: 2]

  @impl true
  def set_state(ports, opts) do
    pid = Keyword.get(opts, :pid)
    res = BP.set_state(ports, state: true)

    get_passed_items(res, ports)
    |> Enum.group_by(& &1.timeout)
    |> Enum.each(fn {timeout, ps} -> Task.start(fn -> postpone_turn_off(ps, timeout, pid) end) end)

    res
  end

  # Privates
  @spec postpone_turn_off(list(integer), integer(), pid()) :: any()
  defp postpone_turn_off(ports, timeout, pid) do
    receive do
    after
      timeout ->
        resp_msg = BP.set_state(ports, state: false)
        if(!is_nil(pid), do: send(pid, {:pulse_result, resp_msg}))
    end
  end
end
