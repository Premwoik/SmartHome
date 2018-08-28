defmodule Core.Actions.CloseSunblinds do
  @moduledoc false

  @behaviour Core.Actions.Action

  @impl true
  def execute(on_off, action, amem) do
    amem
  end

  @impl true
  def init_memory() do
    %{}
  end

end
