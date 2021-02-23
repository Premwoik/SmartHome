defmodule Core.Controllers.Dimmer.TimeDimmer do
  @moduledoc false

  use Core.Controllers.DimmerBeh
  use Witchcraft

  alias Core.Device.Static.Response
  alias Core.Controllers.LightController, as: LC
  #  alias Core.Broadcast, as: Channel

  alias DB.{Port}

  @impl true
  def handle_light_change(%{state: s} = dimmer) do
    #    !true == false => turn on
    #    !false == false => nothing
    #    !true == true => nothing
    #    !false == true => turn off
    if !Port.Dimmer.any_light_on?(dimmer) == s do
      set_state(dimmer, state: !s, deep: false)
    else
      %Response{}
    end
  end

  @impl true
  def set_state(dimmer, ops) do
    s = Keyword.get(ops, :state)
    fill = if(s, do: 100, else: 0)
    set_brightness(dimmer, Keyword.put(ops, :fill, fill))
  end

  @impl true
  def set_brightness(%{state: prev_state} = dimmer, ops) do
    fill = Keyword.get(ops, :fill)
    deep = Keyword.get(ops, :deep, true)
    IO.puts("Set brightness")
    IO.inspect(dimmer)
    IO.inspect(fill)
    IO.inspect(deep)

    with {time, dir} <- Port.Dimmer.fill_to_time(dimmer, fill),
         dimmer <-
           Port.cast(dimmer, state: fill > 0, timeout: time, more: [fill: fill, direction: dir]),
         :ok <- notify_lights(dimmer, prev_state, deep),
         %Response{} = res <- send_brightness(dimmer) do
      map(res |> IO.inspect(), &Port.update/1)
    else
      e ->
        IO.puts("ERROR #{inspect(e)}")
        Response.wrap(:ok, Port.device(dimmer), [dimmer])
    end
  end

  # Privates

  defp send_brightness(%{state: s} = dimmer) when s do
    Core.Device.do_(:set_time_dimmer, [dimmer])
  end

  defp send_brightness(d) do
    Response.wrap({:ok, "The actual brightness is correct"}, Port.device(d), [d])
  end

  def notify_lights(dimmer, prev_state, deep) do
    if(deep and prev_state != dimmer.state) do
      LC.handle_dimmer_change(dimmer)
    else
      :ok
    end

    :ok
  end
end
