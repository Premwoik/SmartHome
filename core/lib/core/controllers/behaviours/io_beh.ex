defmodule Core.Controllers.IOBeh do
  @moduledoc false

  alias Core.Controllers.IOBeh

  defmacro __using__(_) do
    quote([]) do
      @behaviour IOBeh

      def turn_on(items, ops \\ [])
      def turn_on(_, _), do: {:error, "Not implemented yet"}
      def turn_off(items, ops \\ [])
      def turn_off(_, _), do: {:error, "Not implemented yet"}
      def set_state(items, ops \\ [])
      def set_state(_, _), do: {:error, "Not implemented yet"}
      def toggle(items, ops \\ [])
      def toggle(_, _), do: {:error, "Not implemented yet"}
      def read(items, ops \\ [])
      def read(_, _), do: {:error, "Not implemented yet"}

      defoverridable turn_on: 2, turn_off: 2, toggle: 2, read: 2, set_state: 2
    end
  end

  @type def_result() :: :ok | {:error, any(), any()}
  @type content_result() :: {:ok, any()} | {:error, any(), any()}

  @callback turn_on(list(any), ops :: keyword()) :: def_result()
  @callback turn_off(list(any), ops :: keyword()) :: def_result()
  @callback set_state(list(any), ops :: keyword()) :: content_result()
  @callback toggle(list(any), ops :: keyword()) :: def_result()
  @callback read(list(any), ops :: keyword()) :: content_result()
end
