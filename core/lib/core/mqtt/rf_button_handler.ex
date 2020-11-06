defmodule Core.Mqtt.RfButtonHandler do
  @moduledoc false

  def handle_button_click([%DB.RfButton{mode: "page"} = btn], %{pages: pages} = data) do
    controller_id = get_id(btn.name)
    pages_ = Map.update(pages, controller_id, 2, fn i -> next_page_id(i, btn.page_id) end)

    %{data | pages: pages_}
  end

  def handle_button_click([%DB.RfButton{name: name} | _] = btns, %{pages: pages} = data) do
    p = get_page_id(name, pages)
    btn = Enum.find(btns, &(&1.page_id == p))

    get_item(btn)
    |> execute_in_mode(btn.mode)

    data
  end

  ###  Privates

  defp next_page_id(curr_id, max_page_id) do
    if max_page_id == curr_id do
      1
    else
      curr_id + 1
    end
  end

  defp get_id(name) do
    String.split(name, "-")
    |> List.first()
  end

  defp get_page_id(name, pages) do
    controller_id = get_id(name)
    Map.get(pages, controller_id, 1)
  end

  defp get_item(nil), do: nil
  defp get_item(btn) do
    cond do
      !is_nil(btn.port_id) ->
        DB.Port.get_child_struct(btn.port)
      !is_nil(btn.action_id) ->
        btn.action
      !is_nil(btn.task_id) ->
        btn.task
      true -> nil
    end
  end

  defp execute_in_mode(nil, _), do:
    nil
  defp execute_in_mode(item, "toggle"), do:
    Core.Controllers.UniversalController.toggle(item)
  defp execute_in_mode(item, "on"), do:
    Core.Controllers.UniversalController.turn_on(item)
  defp execute_in_mode(item, "off"), do:
    Core.Controllers.UniversalController.turn_off(item)
end
