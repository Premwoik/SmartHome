defmodule UiWeb.RfButtonView do
  use UiWeb, :view
  alias UiWeb.RfButtonView
  import UiWeb.View.Helper

  def render("index.json", %{rf_buttons: rf_buttons}) do
    render_many(rf_buttons, RfButtonView, "rf_button.json")
  end

  def render("show.json", %{rf_button: rf_button}) do
    render_one(rf_button, RfButtonView, "rf_button.json")
  end

  def render("rf_button.json", %{rf_button: rf_button}) do
    %{
      id: rf_button.id,
      on_click_action: render_on_click_action(rf_button.on_click_action),
      name: rf_button.name,
      mode: rf_button.mode,
      page_id: rf_button.page_id,
      key_value: rf_button.key_value,
      "@type": "button"
    }
  end

  def render_on_click_action(actions) when is_list(actions) do
    Enum.map(actions, &render_on_click_action/1)

  end
  def render_on_click_action({page_id, foreign}), do: %{page_id: page_id, action: foreign_view(foreign)}
  def render_on_click_action(other), do: other
end
