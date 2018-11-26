defmodule UiWeb.DashboardChannel do
  @moduledoc false
  use Phoenix.Channel
  require Logger

  def join("dashboard:lobby", _message, socket) do
    Logger.info("Dashboard lobby join")
    {:ok, socket}
  end
end
