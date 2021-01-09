defmodule DB.PageContentSunblind do
  @moduledoc false
  use Ecto.Schema
#  import Ecto.Changeset

#  import Ecto.Query

  alias DB.{Repo, Page, PageContentSunblind}

  schema "page_content_sunblinds" do
    belongs_to(:page, Page)
    belongs_to(:sunblind, DB.Sunblind)
    field(:order, :integer)
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentSunblind, page_id: page_id, sunblind_id: id)

    if d == nil do
      da = %PageContentSunblind{page_id: page_id, sunblind_id: id, order: order}
      Repo.insert(da)
    else
      da = Ecto.Changeset.change(d, action_id: id, order: order)
      Repo.update!(da)
    end
  end
end
