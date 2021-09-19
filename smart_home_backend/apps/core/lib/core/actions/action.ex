defmodule Core.Actions.Action do
  @moduledoc false

  @type state :: :on | :off

  @doc "Execute defined action"
  @callback execute(on_off :: state, action :: map, state :: any) :: {:ok, any()} | :ok

  @doc "Initialize action memory"
  @callback init_state() :: any()
end
