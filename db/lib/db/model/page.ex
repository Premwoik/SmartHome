defmodule DB.Page do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Light, Port, Dimmer, Page}

  @derive {Poison.Encoder, except: [:__meta__]}
  schema "pages" do
    field :name, :string
    field :order, :integer
    field :title, :string
    field :description, :string
    many_to_many :lights, DB.Light, join_through: "page_content_lights", on_delete: :delete_all
    many_to_many :sunblinds, DB.Sunblind, join_through: "page_content_sunblinds", on_delete: :delete_all
    many_to_many :dimmers, DB.Dimmer, join_through: "page_content_dimmers", on_delete: :delete_all
    many_to_many :ports, DB.Port, join_through: "page_content_ports", on_delete: :delete_all
    many_to_many :actions, DB.Action, join_through: "page_content_actions", on_delete: :delete_all
    many_to_many :tasks, DB.Task, join_through: "page_content_tasks", on_delete: :delete_all
  end

  def changeset(page, attrs) do
    page
    |> cast(attrs, [:name, :order, :title, :description])
  end
end
