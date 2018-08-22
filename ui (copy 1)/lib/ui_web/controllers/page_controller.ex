defmodule UIWeb.PageController do
  use UIWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end


  """

  """
  def all(conn, _params) do
    data = %{dimmers: DB.Dao.json_dimmers, sunblinds: DB.Dao.json_sunblind}
    json conn, data
  end

  def sunblinds(conn, _params) do
    data = DB.Dao.json_sunblind
    json conn, data
  end

  def dimmers(conn, _params) do
    data = DB.Dao.json_dimmers
    json conn, data
  end


end
