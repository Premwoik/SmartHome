defmodule Core.Actions.Action do
  @moduledoc false

  @type state :: :on | :off

  @doc "Execute defined action"
  @callback execute(on_off :: state, action :: map, state :: any) :: map

  @doc "Initialize action memory"
  @callback init_state() :: any()
end
