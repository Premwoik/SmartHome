defmodule Core.Controllers.DimmerController do
  @moduledoc false

  use Core.Controllers.IOBeh
  alias Core.Controllers.IOBeh

  alias DB.{Dimmer}
  alias Core.Controllers.DimmerController
  import Core.Controllers.Universal, only: [flatten_result: 1]

  defmacro __using__(_) do
    quote([]) do
      @behaviour DimmerController
      def set_state(_, _), do: {:error, "Not implemented yet"}
      def get_state(_), do: {:error, "Not implemented yet"}
      def set_brightness(_, _), do: {:error, "Not implemented yet"}
      def set_white_brightness(_, _), do: {:error, "Not implemented yet"}
      def set_color(_, _, _, _), do: {:error, "Not implemented yet"}
      def notify_light_change(_), do: {:error, "Not implemented yet"}

      defoverridable set_state: 2,
                     get_state: 1,
                     set_brightness: 2,
                     set_white_brightness: 2,
                     set_color: 4,
                     notify_light_change: 1
    end
  end

  @callback set_state(%Dimmer{}, state :: bool()) :: :ok | {:error, any()}
  @callback set_brightness(%Dimmer{}, fill :: integer()) :: :ok | {:error, any()}
  @callback set_white_brightness(%Dimmer{}, fill :: integer()) :: :ok | {:error, any()}
  @callback set_color(%Dimmer{}, r :: integer(), g :: integer(), b :: integer()) ::
              :ok | {:error, any()}
  @callback get_state(%Dimmer{}) :: {:ok, bool()} | {:error, String.t()}
  @callback notify_light_change(%Dimmer{}) :: :ok | {:error, String.t()}

  #  def read(dimmer) do
  #    [dimmer.port.device, dimmer.port]
  #    |> Device.do_r(:status)
  #    #    |> process_response(dimmer) TODO handle saving response to DB
  #    :ok
  #  end

  def get_state(dimmer) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.get_state(dimmer)
    end
  end

  @impl IOBeh
  def toggle(dimmer, _ops) when is_list(dimmer) == false do
    with {:ok, mod} <- get_module(dimmer),
         {:ok, state} <- mod.get_state(dimmer) do
      mod.set_state(dimmer, !state)
    end
  end

  def toggle(dimmers, _ops),
    do:
      Enum.map(dimmers, &toggle(&1))
      |> flatten_result()

  @impl IOBeh
  def turn_on(dimmer, _ops) when is_list(dimmer) == false do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_state(dimmer, true)
    end
  end

  def turn_on(dimmers, _ops),
    do:
      Enum.map(dimmers, &turn_on(&1))
      |> flatten_result()

  @impl IOBeh
  def turn_off(dimmer, _ops) when is_list(dimmer) == false do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_state(dimmer, false)
    end
  end

  def turn_off(dimmers, _ops),
    do:
      Enum.map(dimmers, &turn_off(&1))
      |> flatten_result()

  def set_color(dimmer, red, green, blue) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_color(dimmer, red, green, blue)
    end
  end

  def set_white_brightness(dimmer, fill) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_white_brightness(dimmer, fill)
    end
  end

  def set_brightness(dimmer, fill) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.set_brightness(dimmer, fill)
    end
  end

  def notify_light_change(dimmer, _s \\ nil) do
    with {:ok, mod} <- get_module(dimmer) do
      mod.notify_light_change(dimmer)
    end
  end

  # Privates

  def get_module(%{
        port: %{
          type: t,
          mode: m
        }
      }) do
    case t do
      "dimmer2" ->
        {:ok, Core.Controllers.Dimmer.Time2Dimmer}

      "dimmer" ->
        case m do
          "output" ->
            {:ok, Core.Controllers.Dimmer.TimeDimmer}

          "output_pwm" ->
            {:ok, Core.Controllers.Dimmer.PwmDimmer}

          _ ->
            {:error, "Wrong mode"}
        end

      "dimmer_rgb" <> _ ->
        {:ok, Core.Controllers.Dimmer.RgbDimmer}

      _ ->
        {:error, "Wrong type"}
    end
  end
end
