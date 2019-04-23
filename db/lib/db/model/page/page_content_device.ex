defmodule DB.PageContentDevice do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentDevice}

  schema "page_content_devices" do
    belongs_to :page, Page
    belongs_to :device, DB.Device
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentDevice, page_id: page_id, device_id: id)
    if d == nil do
      da = %PageContentDevice{page_id: page_id, device_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, device_id: id, order: order)
      Repo.update! da
    end
  end

end
