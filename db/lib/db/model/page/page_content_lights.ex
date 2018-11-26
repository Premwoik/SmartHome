defmodule DB.PageContentLight do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentLight}

  schema "page_content_lights" do
    belongs_to :page, Page
    belongs_to :light, Light
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentLight, page_id: page_id, light_id: id)
    if d == nil do
      da = %PageContentLight{page_id: page_id, light_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, light_id: id, order: order)
      Repo.update! da
    end
  end

end
