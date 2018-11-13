defmodule DB.Page do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page, PageContent}

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "pages" do
    field :name, :string
    field :number, :integer
    field :title, :string
    field :description, :string
    has_many :subgroups, DB.Page, foreign_key: :parent_id
    belongs_to :parent, DB.Page
    has_one :content, DB.PageContent
  end


  def preload_all do
    [:subgroups, :parent, content: PageContent.preload_all]
  end

  def all_short_info() do
    from(
      p in Page,
      where: is_nil(p.parent_id),
      select: %{
        id: p.id,
        name: p.name,
        number: p.number
      }
    )
    |> Repo.all()
    |> Enum.map(&(Map.put(&1, :subpages, get_subpages(&1.id))))
  end

  defp get_subpages(page_id) do
    from(
      p in Page,
      where: p.parent_id == ^page_id,
      select: %{
        id: p.id,
        name: p.name,
        number: p.number
      }
    )
    |> Repo.all()
  end

  def all() do
    Repo.all(Page)
    |> Repo.preload(preload_all())
  end

  def get(id) do
    Repo.get(Page, id)
    |> Repo.preload(preload_all())
  end

  def page_view(id) do
    (
      from p in Page,
           where: p.id == ^id,
           select: %{
             id: p.id,
             name: p.name,
             title: p.title,
             description: p.description,
             subpages: [],
             number: p.number,

           })
    |> Repo.all()
    |> fn [x] ->
      content =
        PageContent.get_content_id(x.id)
        |> PageContent.get_cont_list()
      Map.put(x, :content, content)
       end.()
  end


end
