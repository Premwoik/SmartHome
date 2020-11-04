defmodule UiWeb.RfButtonController do
  use UiWeb, :controller

  alias DB.RfButton, as: RfButton
  alias Ui.RfButtonAdmin, as: Admin

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    rf_buttons = Admin.list_rf_buttons()
    render(conn, "index.json", rf_buttons: rf_buttons)
  end

  def create(conn, %{"rf_button" => rf_button_params}) do
    with {:ok, %RfButton{} = rf_button} <- Admin.create_rf_button(rf_button_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.rf_button_path(conn, :show, rf_button))
      |> render("show.json", rf_button: rf_button)
    end
  end

  def show(conn, %{"id" => id}) do
    rf_button = Admin.get_rf_button!(id)
    render(conn, "show.json", rf_button: rf_button)
  end

  def update(conn, %{"id" => id, "rf_button" => rf_button_params}) do
    rf_button = Admin.get_rf_button!(id)

    with {:ok, %RfButton{} = rf_button} <- Admin.update_rf_button(rf_button, rf_button_params) do
      render(conn, "show.json", rf_button: rf_button)
    end
  end

  def delete(conn, %{"id" => id}) do
    rf_button = Admin.get_rf_button!(id)

    with {:ok, %RfButton{}} <- Admin.delete_rf_button(rf_button) do
      send_resp(conn, :no_content, "")
    end
  end
end
