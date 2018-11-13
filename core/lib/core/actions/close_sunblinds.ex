defmodule Core.Actions.CloseSunblinds do
  @moduledoc false
  require Logger
  alias DB.{Sunblind}
  alias Core.Controllers.SunblindController

  @behaviour Core.Actions.Action

  @impl true
  def init_memory() do
    %{}
  end

  @impl true
  def execute(:up, _action, amem) do
    Sunblind.get_type("only_close")
    |> SunblindController.close()
    amem
  end
  def execute(:down, _action, amem) do
    Sunblind.get_type("only_close")
    |> SunblindController.open()
    amem
  end

end
