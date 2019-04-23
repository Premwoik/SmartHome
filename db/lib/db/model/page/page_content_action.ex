defmodule DB.PageContentAction do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentAction}

  schema "page_content_actions" do
    belongs_to :page, Page
    belongs_to :action, DB.Action
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentAction, page_id: page_id, action_id: id)
    if d == nil do
      da = %PageContentAction{page_id: page_id, action_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, action_id: id, order: order)
      Repo.update! da
    end
  end

end
