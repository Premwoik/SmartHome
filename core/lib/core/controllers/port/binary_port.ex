defmodule Core.Controllers.Port.BinaryPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController
  alias DB.Port
  use Witchcraft

  @impl true
  def set_state(ports, state: state) do
    Port.cast(ports, state: state)
    |> Core.Device.do_r(:set_outputs)
    |> map(&Port.update(&1, state: state))
  end
end
