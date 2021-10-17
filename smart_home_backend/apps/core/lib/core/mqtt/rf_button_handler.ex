defmodule Core.Mqtt.RfButtonHandler do
  @moduledoc false

  alias DB.Data.RfButton

  def handle_button_click(%RfButton{mode: :page} = btn, %{pages: pages} = data) do
    controller_id = extract_controller_name(btn.name)
    pages_ = Map.update(pages, controller_id, 2, fn i -> next_page_id(i, btn.page) end)

    %{data | pages: pages_}
  end

  def handle_button_click(%RfButton{name: name} = btn, %{pages: pages} = data) do
    page = get_current_page(name, pages)

    RfButton.click_action(btn, page)
    |> execute_in_mode(btn.mode)

    data
  end

  def handle_button_click(nil, data), do: data

  ###  Privates

  defp next_page_id(curr_id, max_page_id) do
    if max_page_id == curr_id do
      1
    else
      curr_id + 1
    end
  end

  defp extract_controller_name(name) do
    String.split(name, "-")
    |> List.first()
  end

  defp get_current_page(name, pages) do
    name = extract_controller_name(name)
    Map.get(pages, name, 1)
  end

  defp execute_in_mode(nil, _), do: :ok
  defp execute_in_mode(item, nil), do: Core.Controller.toggle([item])
  defp execute_in_mode(item, :toggle), do: Core.Controller.toggle([item])
  defp execute_in_mode(item, :on), do: Core.Controller.turn_on([item])
  defp execute_in_mode(item, :off), do: Core.Controller.turn_off([item])
end
