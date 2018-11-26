defmodule DB.PageContentPort do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentPort}

  schema "page_content_ports" do
    belongs_to :page, Page
    belongs_to :port, DB.Port
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentPort, page_id: page_id, port_id: id)
    if d == nil do
      da = %PageContentPort{page_id: page_id, port_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, port_id: id, order: order)
      Repo.update! da
    end
  end
end
