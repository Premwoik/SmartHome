defmodule UiWeb.RfButtonView do
  use UiWeb, :view
  alias UiWeb.RfButtonView

  def render("index.json", %{rf_buttons: rf_buttons}) do
    render_many(rf_buttons, RfButtonView, "rf_button.json")
  end

  def render("show.json", %{rf_button: rf_button}) do
    render_one(rf_button, RfButtonView, "rf_button.json")
  end

  def render("rf_button.json", %{rf_button: rf_button}) do
    %{
      id: rf_button.id,
      port_id: rf_button.port_id,
      action_id: rf_button.action_id,
      task_id: rf_button.task_id,
      name: rf_button.name,
      mode: rf_button.mode,
      page_id: rf_button.page_id,
      key_value: rf_button.key_value,
      "@type": "button"
    }
  end
end
