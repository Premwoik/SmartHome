defmodule UiWeb.SettingsView do
  use UiWeb, :view

  alias DB.Data.RfButton
  alias DB.Data.Port
  alias DB.Data.Action
  import Phoenix.HTML

  def time_to_str(t = {_, _, _}) do
    Time.to_string(Time.from_erl!(t))
  end

  def custom_td_class(id, pilot, selected) do
    selected_id = Map.get(selected[pilot], "id", "0")

    if selected_id == id do
      "px-4 border bg-green-300"
    else
      "px-4 border"
    end
  end

  def rf_button_view(%RfButton{mode: :page}) do
    raw("<p>* - Strona</p>")
  end

  def rf_button_view(rf_button) do
    actions = RfButton.get_actions(rf_button)

    txt =
      Enum.map(actions, fn
        {page, %Port{name: name}} -> raw("<p>#{page} - #{name}</p>")
        {page, %Action{name: name}} -> raw("<p>#{page} - #{name}</p>")
      end)

    case txt do
      [] -> raw("<p>brak</p>")
      _ -> txt
    end
  end
end
