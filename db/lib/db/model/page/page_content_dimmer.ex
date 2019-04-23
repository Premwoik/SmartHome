defmodule DB.PageContentDimmer do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentDimmer}

  schema "page_content_dimmers" do
    belongs_to :page, Page
    belongs_to :dimmer, DB.Dimmer
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentDimmer, page_id: page_id, dimmer_id: id)
    if d == nil do
      da = %PageContentDimmer{page_id: page_id, dimmer_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, dimmer_id: id, order: order)
      Repo.update! da
    end
  end
end
