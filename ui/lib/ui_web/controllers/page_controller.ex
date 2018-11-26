defmodule UiWeb.PageController do
  use UiWeb, :controller


  alias DB.{Repo, Page, PageContent}

  def index(conn, _params) do
    render conn, "index.html"
  end


  """

  """
  def all(conn, _params) do
    data = Page.all()
    json conn, data
  end

  def short(conn, _params) do
    data = Page.all_short_info()
    json conn, data
  end


  def page(conn, %{"id" => id}) do
    id
    |> Page.page_view()
    |> fn data -> json conn, data end.()

  end

end
