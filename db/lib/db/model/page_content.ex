defmodule DB.PageContent do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  import Ecto.Query

  alias DB.{Page, PageContent, Repo, Port, Light, Sunblind, Dimmer, Action, Task, TaskType}

  schema "page_contents" do
    belongs_to :page, DB.Page
    field :number, :integer
    many_to_many :lights, DB.Light, join_through: "page_content_lights", on_delete: :delete_all
    many_to_many :sunblinds, DB.Sunblind, join_through: "page_content_sunblinds", on_delete: :delete_all
    many_to_many :dimmers, DB.Dimmer, join_through: "page_content_dimmers", on_delete: :delete_all
    many_to_many :ports, DB.Port, join_through: "page_content_ports", on_delete: :delete_all
    many_to_many :actions, DB.Action, join_through: "page_content_actions", on_delete: :delete_all
    many_to_many :tasks, DB.Task, join_through: "page_content_tasks", on_delete: :delete_all
  end

  def preload_all do
    [
      :actions,
      :tasks,
      :ports,
      lights: [:port],
      sunblinds: [:port],
      dimmers: [:port]
    ]
  end


  def get_content_id(page_id) do
    from(c in PageContent, where: c.page_id == ^page_id, select: c.id)
    |> Repo.all()
    |> fn [x] -> x end.()
  end





  def get_cont_list(page_content_id) do
    view_format_ports(page_content_id)
    ++
    view_format_lights(page_content_id)
    ++
    view_format_dimmers(page_content_id)
    ++
    view_format_sunblinds(page_content_id)
    ++
    view_format_actions(page_content_id)
    ++
    view_format_tasks(page_content_id)
    |> Enum.sort_by(&(&1.order))
  end


  def view_format_ports(page_content_id) do
    from(
      p in Port,
      join: c in "page_content_ports",
      on: c.port_id == p.id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: p.id,
        state: p.state,
        order: c.order,
        name: p.name,
        port_type: p.type,
        port: ""
      }
    )
    |> Repo.all
  end

  def view_format_lights(page_content_id) do
    from(
      l in Light,
      join: c in "page_content_lights",
      on: c.light_id == l.id,
      join: p in Port,
      on: p.id == l.port_id,
      left_join: d in Dimmer,
      on: l.dimmer_id == d.id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: l.id,
        fill: d.fill,
        order: c.order,
        state: p.state,
        name: p.name,
        light: ""
      }
    )
    |> Repo.all
  end

  def view_format_sunblinds(page_content_id) do
    from(
      s in Sunblind,
      join: c in "page_content_sunblinds",
      on: c.sunblind_id == s.id,
      join: p in Port,
      on: p.id == s.port_id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: s.id,
        state: s.state,
        sunblind_type: s.type,
        position: s.position,
        order: c.order,
        name: p.name,
        sunblind: ""
      }
    )
    |> Repo.all
  end

  def view_format_dimmers(page_content_id) do

    load_lights =
      fn id ->
        from(
          l in Light,
          join: p in Port,
          on: p.id == l.port_id,
          where: l.dimmer_id == ^id,
          select: %{
            id: l.id,
            state: p.state,
            name: p.name,
          }
        )
        |> Repo.all()
      end

    from(
      d in Dimmer,
      join: c in "page_content_dimmers",
      on: c.dimmer_id == d.id,
      join: p in Port,
      on: p.id == d.port_id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: d.id,
        fill: d.fill,
        order: c.order,
        name: p.name,
        dimmer: ""
      }
    )
    |> Repo.all
    |> Enum.map(&(Map.put(&1, :lights, load_lights.(&1.id))))
  end

  def view_format_actions(page_content_id) do
    from(
      a in Action,
      join: c in "page_content_actions",
      on: c.action_id == a.id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: a.id,
        name: "TODO",
        order: c.order,
        function: a.function,
        state: a.active,
        action: ""
      }
    )
    |> Repo.all()
  end

  def view_format_tasks(page_content_id) do
    from(
      t in Task,
      join: c in "page_content_tasks",
      on: c.task_id == t.id,
      join: ty in TaskType,
      on: ty.id == t.type_id,
      where: c.page_content_id == ^page_content_id,
      select: %{
        id: t.id,
        order: c.order,
        name: "TODO",
        status: t.status,
        task: ""
      }
    )
    |> Repo.all()
  end

end
