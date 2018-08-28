defmodule Core.Actions.Action do
  @moduledoc false

  @doc "Execute defined action"
  @callback execute(on_off :: boolean, action :: map, amem :: map) :: map

  @doc "Initialize action memory"
  @callback init_memory() :: map
end
