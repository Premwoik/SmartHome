defmodule UiWeb.LoggsLive do
  use UiWeb, :live_view

  alias Core.Cache.LoggsCache

  def mount(_params, _session, socket) do
    loggs = LoggsCache.get_latest()
    {:ok, assign(socket, page: 0, num: 100, loggs: loggs)}
  end

  def handle_event("update_filters", %{"page" => %{"num" => num, "id" => id}}, socket) do
    {id, ""} = Integer.parse(id)
    {num, ""} = Integer.parse(num)
    loggs = LoggsCache.get_page(id, num)

    {:noreply, assign(socket, page: id, num: num, loggs: loggs)}
  end
end
