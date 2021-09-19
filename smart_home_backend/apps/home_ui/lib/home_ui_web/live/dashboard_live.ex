defmodule HomeUiWeb.DashboardLive do
  use HomeUiWeb, :live_view

  alias Core.Controllers.{SunblindController, DimmerController, PortController}
  alias DB.Data.Page

  @topic "dashboard:lobby"
  @monitor_topic "monitor:lobby"

  def mount(_params, _session, socket) do
    HomeUiWeb.Endpoint.subscribe(@topic)
    HomeUiWeb.Endpoint.subscribe(@monitor_topic)
    pages = Page.short_info() 

    {:ok,
     assign(socket,
       pages_short: pages,
       lights: [],
       dimmers: [],
       sunblinds: [],
       sensors: [],
       plan: "attic"
     )}
  end

  def handle_params(%{"page_id" => page_id}, _url, socket) do
    {id, _} = Integer.parse(page_id)
    page = Page.get(id)

    if !is_nil(page) do
      lights = Page.lights(page)
      dimmers = Page.dimmers(page)
      sunblinds = Page.sunblinds(page)
      sensors = []
      plan = get_plan(id)

      {:noreply,
       assign(socket,
         lights: lights,
         dimmers: dimmers,
         sunblinds: sunblinds,
         sensors: sensors,
         plan: plan
       )}
    else
      {:noreply, socket}
    end
  end

  def handle_params(_params, _url, socket) do
    case socket.assigns[:pages_short] do
      [] ->
        {:noreply, socket}

      pages ->
        %{id: id} = hd(pages)
        {:noreply, push_patch(socket, to: "/dash/#{id}")}
    end
  end

  def get_plan(id) do
    case id do
      2 -> "parter"
      3 -> "living_room"
      4 -> "first_floor"
      _otherwise -> "attic"
    end
  end

  def handle_event("toggle_light", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    lights = socket.assigns[:lights]

    lights =
      Enum.map(lights, fn port ->
        if(port.id == id) do
          %{ok: [light]} = PortController.toggle([port])
          broadcast_from_self!(%{type: :light, item: light})
          light
        else
          port
        end
      end)

    # HomeUiWeb.Endpoint.broadcast_from!(self(), @topic, "port_update", id)
    {:noreply, assign(socket, lights: lights)}
  end

  def handle_event("toggle_dimmer", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    items = socket.assigns[:dimmers]

    items =
      Enum.map(items, fn port ->
        if(port.id == id) do
          %{ok: [dimmer]} = DimmerController.toggle([port])
          broadcast_from_self!(%{type: :dimmer, item: dimmer})
          dimmer
        else
          port
        end
      end)

    {:noreply, assign(socket, dimmers: items)}
  end

  def handle_event("toggle_dimmer_light", %{"value" => id, "dimmer_id" => dimmer_id}, socket) do
    {id, _} = Integer.parse(id)
    {dimmer_id, _} = Integer.parse(dimmer_id)

    items = socket.assigns[:dimmers]

    items =
      Enum.map(items, fn port ->
        if(port.id == dimmer_id) do
          lights =
            Enum.map(port.more.lights, fn light ->
              if(light.id == id) do
                %{ok: [light]} = PortController.toggle([light])
                broadcast(%{type: :light, item: light})
                light
              else
                light
              end
            end)

          dimmer = put_in(port.state.lights, lights)
          broadcast_from_self!(%{type: :dimmer, item: dimmer})
          dimmer
        else
          port
        end
      end)

    # socket = put_flash(socket, :error, "It worked!")
    {:noreply, assign(socket, dimmers: items)}
  end

  def handle_event("fill_changed", %{"dimmer_id" => id, "value" => fill}, socket) do
    {fill, _} = Integer.parse(fill)
    {id, _} = Integer.parse(id)

    items = socket.assigns[:dimmers]

    items =
      Enum.map(items, fn port ->
        if(port.id == id) do
          %{ok: [dimmer]} = DimmerController.set_brightness(port, fill)
          broadcast_from_self!(%{type: :dimmer, item: dimmer})
          dimmer
        else
          port
        end
      end)

    {:noreply, assign(socket, dimmers: items)}
  end

  def handle_event("white_fill_changed", %{"dimmer_id" => id, "value" => fill}, socket) do
    {fill, _} = Integer.parse(fill)
    {id, _} = Integer.parse(id)

    items = socket.assigns[:dimmers]

    items =
      Enum.map(items, fn port ->
        if(port.id == id) do
          %{ok: [dimmer]} = DimmerController.set_white_brightness(port, fill)
          broadcast_from_self!(%{type: :dimmer, item: dimmer})
          dimmer
        else
          port
        end
      end)

    {:noreply, assign(socket, dimmers: items)}
  end

  def handle_event("toggle_sunblind", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    items =
      socket.assigns[:sunblinds]
      |> Enum.map(fn port ->
        if(port.id == id) do
          %{ok: [sunblind]} = SunblindController.toggle([port])
          broadcast_from_self!(%{type: :sunblind, item: sunblind})
          sunblind
        else
          port
        end
      end)

    {:noreply, assign(socket, sunblinds: items)}
  end

  def handle_info(
        %{
          topic: @monitor_topic,
          event: "active:inputs",
          payload: %{numbers: up}
        },
        socket
      ) do
    sensors =
      socket.assigns[:sensors]
      |> Enum.map(fn s ->
        if s.id in Enum.map(up, & &1.id) do
          %{s | state: true}
        else
          %{s | state: false}
        end
      end)

    {:noreply, assign(socket, :sensors, sensors)}
  end

  def handle_info(
        %{topic: @topic, event: "object:updated", payload: %{type: type, item: item}},
        socket
      ) do
    res = type_to_resource(type)

    items =
      socket.assigns[res]
      |> Enum.map(&if(&1.id == item.id, do: item, else: &1))

    {:noreply, assign(socket, res, items)}
  end

  defp type_to_resource(type) do
    case type do
      :light -> :lights
      :dimmer -> :dimmers
      :sunblind -> :sunblinds
      :sensor -> :sensors
    end
  end

  defp broadcast_from_self!(data) do
    HomeUiWeb.Endpoint.broadcast_from!(self(), @topic, "object:updated", data)
  end

  defp broadcast(data) do
    HomeUiWeb.Endpoint.broadcast!(@topic, "object:updated", data)
  end
end
