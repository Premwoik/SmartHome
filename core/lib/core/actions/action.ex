defmodule Core.Actions.Action do
  @moduledoc false

  @type state :: :on | :off

  @doc "Execute defined action"
  @callback execute(on_off :: state, action :: map, amem :: map) :: map

  @doc "Initialize action memory"
  @callback init_memory() :: map
end
