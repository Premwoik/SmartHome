defmodule UiWeb.DashboardLive do
  use UiWeb, :live_view

  alias Core.Device.Static.Response
  alias Core.DimmerController
  alias Core.PortController
  alias Core.SunblindController
  alias DB.Data.Page
  alias DB.Data.Port
  alias DB.Proc.PortListProc

  @topic "dashboard:lobby"
  @monitor_topic "monitor:lobby"

  def mount(_params, _session, socket) do
    UiWeb.Endpoint.subscribe(@topic)
    UiWeb.Endpoint.subscribe(@monitor_topic)
    pages = Page.short_info()

    {:ok,
     assign(socket,
       pages_short: pages,
       lights: [],
       dimmers: [],
       sunblinds: [],
       sensors: [],
       others: [],
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
      sensors = Page.motion_sensors(page)
      others = Page.others(page)
      plan = get_plan(id)

      {:noreply,
       assign(socket,
         lights: lights,
         dimmers: dimmers,
         sunblinds: sunblinds,
         sensors: sensors,
         others: others,
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

    socket =
      update_resource(:lights, id, socket, fn port, s ->
        {s, PortController.toggle([port])}
      end)

    {:noreply, socket}
  end

  def handle_event("toggle_other", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:others, id, socket, fn port, s ->
        {s, PortController.toggle([port])}
      end)

    {:noreply, socket}
  end

  def handle_event("toggle_dimmer", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:dimmers, id, socket, fn port, s ->
        case DimmerController.toggle([port], broadcast: false) do
          %{ok: [dimmer]} = res ->
            lights =
              dimmer.state
              |> Map.get("lights", [])
              |> Enum.map(fn light ->
                PortListProc.update_state!(light.id, %{"value" => dimmer.state["value"]})
                |> broadcast_from_self!()
              end)

            Port.put_state(dimmer, "lights", lights)
            |> broadcast_from_self!()
            |> (fn d -> {s, %{res | ok: [d]}} end).()

          res ->
            {s, res}
        end
      end)

    {:noreply, socket}
  end

  def handle_event("toggle_dimmer_light", %{"value" => id, "dimmer_id" => dimmer_id}, socket) do
    {id, _} = Integer.parse(id)
    {dimmer_id, _} = Integer.parse(dimmer_id)

    socket =
      update_resource(:dimmers, dimmer_id, socket, fn port, s ->
        {s, lights} =
          update_resource(port.state["lights"], id, s, fn light, s ->
            {s, PortController.toggle([light])}
          end)

        dimmer =
          Port.put_state(port, "lights", lights)
          |> broadcast_from_self!()

        {s, Response.ok([dimmer])}
      end)

    {:noreply, socket}
  end

  def handle_event("color_changed", %{"dimmer" => %{"id" => id, "color" => color}}, socket) do
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:dimmers, id, socket, fn port, s ->
        {s, DimmerController.set_color([port], color)}
      end)

    {:noreply, socket}
  end

  def handle_event("fill_changed", %{"dimmer" => %{"id" => id, "fill" => fill}}, socket) do
    {fill, _} = Integer.parse(fill)
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:dimmers, id, socket, fn port, s ->
        {s, DimmerController.set_brightness([port], fill)}
      end)

    {:noreply, socket}
  end

  def handle_event("white_fill_changed", %{"dimmer" => %{"id" => id, "white" => fill}}, socket) do
    {fill, _} = Integer.parse(fill)
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:dimmers, id, socket, fn port, s ->
        {s, DimmerController.set_white_brightness([port], fill)}
      end)

    {:noreply, socket}
  end

  def handle_event("toggle_sunblind", %{"value" => id}, socket) do
    {id, _} = Integer.parse(id)

    socket =
      update_resource(:sunblinds, id, socket, fn port, s ->
        {s, SunblindController.toggle([port])}
      end)

    {:noreply, socket}
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
        value = s.id in Enum.map(up, & &1.id)
        Port.put_state(s, "value", value)
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
      :circut -> :others
      :custom -> :others
    end
  end

  defp broadcast_from_self!(port) do
    UiWeb.Endpoint.broadcast_from!(self(), @topic, "object:updated", %{
      type: port.type,
      item: port
    })

    port
  end

  @spec update_resource(atom(), integer(), any(), (Port.t(), map() -> {any(), Response.t()})) ::
          any()
  def update_resource(resource, id, socket, fun) when is_atom(resource) do
    items = socket.assigns[resource]
    {socket, items} = update_resource(items, id, socket, fun)
    assign(socket, resource, items)
  end

  def update_resource(resource, id, socket, fun) do
    List.foldl(resource, {socket, []}, fn p, {socket, items} ->
      if p.id == id do
        case fun.(p, socket) do
          {socket, %{ok: []}} ->
            socket = put_flash(socket, :error, "Nie udało się nawiązać połączenia z urządzeniem!")
            {socket, [p | items]}

          {socket, %{ok: [p]}} ->
            {socket, [p | items]}
        end
      else
        {socket, [p | items]}
      end
    end)
    |> (fn {socket, items} ->
          {socket, Enum.reverse(items)}
        end).()
  end
end
