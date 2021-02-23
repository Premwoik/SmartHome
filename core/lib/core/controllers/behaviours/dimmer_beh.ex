defmodule Core.Controllers.DimmerBeh do
  @moduledoc false

  alias DB.Port

  defmacro __using__(_) do
    quote([]) do
      @behaviour Core.Controllers.DimmerBeh
      def set_state(device, ops \\ [])
      def set_state(_, _), do: {:error, "Not implemented yet"}
      def set_brightness(device, ops \\ [])
      def set_brightness(_, _), do: {:error, "Not implemented yet"}
      def set_white_brightness(device, ops \\ [])
      def set_white_brightness(_, _), do: {:error, "Not implemented yet"}
      def set_color(device, ops \\ [])
      def set_color(_, _), do: {:error, "Not implemented yet"}

      def handle_light_change(_), do: :ok

      defoverridable set_state: 2,
                     set_brightness: 2,
                     set_white_brightness: 2,
                     set_color: 2,
                     handle_light_change: 1
    end
  end

  alias Core.Device.Static.Response

  @type result :: %Response{}

  @callback set_state(%Port{}, ops :: keyword()) :: result()
  @callback set_brightness(%Port{}, ops :: keyword()) :: result()
  @callback set_white_brightness(%Port{}, ops :: keyword()) :: result()
  @callback set_color(%Port{}, ops :: keyword()) :: result()
  @callback handle_light_change(%Port{}) :: :ok | {:error, String.t()}
end
