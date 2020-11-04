defmodule UiWeb.DashboardController do
  use UiWeb, :controller

  alias Ui.DashboardAdmin
  alias Ui.DashboardAdmin.Dashboard
  alias DB.Page

  action_fallback UiWeb.FallbackController

  def index(conn, _params) do
    dashboards = DashboardAdmin.list_dashboards()
    render(conn, "index.json", dashboards: dashboards)
  end

  def create(conn, %{"dashboard" => dashboard_params}) do

    with {:ok, dashboard} <- DashboardAdmin.create_dashboard(dashboard_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.dashboard_path(conn, :show, dashboard))
      |> render("show.json", dashboard: dashboard)
    end
  end

  def show(conn, %{"id" => id}) do
    dashboard = DashboardAdmin.get_dashboard!(id)
    render(conn, "show.json", dashboard: dashboard)
  end

  def update(conn, %{"id" => id, "dashboard" => dashboard_params}) do
    dashboard = DashboardAdmin.get_dashboard!(id)

    with {:ok, dashboard} <- DashboardAdmin.update_dashboard(dashboard, dashboard_params) do
      render(conn, "show.json", dashboard: dashboard)
    end
  end

  def delete(conn, %{"id" => id}) do
    dashboard = DashboardAdmin.get_dashboard_short!(id)
    with {:ok, %Page{}} <- DashboardAdmin.delete_dashboard(dashboard) do
      send_resp(conn, :no_content, "")
    end
  end

  def short(conn, _params) do
    data = DashboardAdmin.list_dashboards_short()
    render(conn, "index.json", dashboards_short: data)
  end


  def view(conn, %{"id" => id}) do
    d = DashboardAdmin.get_dashboard!(id)
    render(conn, "show.json", dash_dashboard: d)
  end

end
