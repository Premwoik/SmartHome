defmodule UiWeb.PageController do
  use UiWeb, :controller


  alias DB.{Repo, Page, PageContent}

  def index(conn, _params) do
    render conn, "index.html"
  end

end
