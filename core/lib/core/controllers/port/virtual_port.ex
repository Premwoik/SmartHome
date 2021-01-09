defmodule Core.Controllers.Port.VirtualPort do
  @moduledoc false

  @behaviour Core.Controllers.BasicController

  @impl true
  def set_state(_ports, _ops) do
    {:error, "Not implemented yet"}
  end

  #  Privates
end
