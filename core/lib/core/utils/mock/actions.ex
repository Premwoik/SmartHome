defmodule Core.Utils.Mock.Actions do
  @moduledoc false

  require Logger

  def activate_up(ids),
    do: Logger.info("Activating actions up: #{inspect(ids, charlists: :as_lists)}")

  def activate_down(ids),
    do: Logger.info("Activating actions down: #{inspect(ids, charlists: :as_lists)}")
end
