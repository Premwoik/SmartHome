defmodule DB.PageContentTask do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContentTask}

  schema "page_content_tasks" do
    belongs_to :page, Page
    belongs_to :task, DB.Task
    field :order, :integer
  end

  def insert_or_update(page_id, [id, order]) do
    d = Repo.get_by(PageContentTask, page_id: page_id, task_id: id)
    if d == nil do
      da = %PageContentTask{page_id: page_id, task_id: id, order: order}
      Repo.insert da
    else
      da = Ecto.Changeset.change(d, task_id: id, order: order)
      Repo.update! da
    end
  end
end
