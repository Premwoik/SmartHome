defmodule Core.Telemetry.Helpers do
  require Logger

  @spec safe_code((() -> any())) :: any()
  def safe_code(fun) do
    try do
      fun.()
    rescue
      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
    end
  end
end
