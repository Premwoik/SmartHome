defmodule Core.Controllers.Controller do
  @moduledoc false

  @callback turn_on(map)  :: any
  @callback turn_off(map)  :: any
  @callback toggle(map)  :: any

end
