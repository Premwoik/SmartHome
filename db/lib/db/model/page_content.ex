defmodule DB.PageContent do
  # TODO check if it could be removed
  @moduledoc false
  use Ecto.Schema

  import Ecto.Query

  alias DB.{Repo, Port, Light, Sunblind, Dimmer, Action, Task}

  def preload_all do
    [
      :actions,
      :tasks,
      :ports,
      :devices,
      lights: [:port],
      sunblinds: [:port],
      dimmers: [:port]
    ]
  end

  def get_cont_list(page_id) do
    (view_format_ports(page_id) ++
       view_format_lights(page_id) ++
       view_format_dimmers(page_id) ++
       view_format_sunblinds(page_id) ++
       view_format_actions(page_id) ++ view_format_tasks(page_id))
    |> Enum.sort_by(fn {o, _} -> o end)
    |> Enum.map(fn {_, x} -> x end)
  end

  def view_format_ports(page_id) do
    from(
      p in Port,
      join: c in "page_content_ports",
      on: c.port_id == p.id,
      where: c.page_id == ^page_id,
      select: {c.order, p}
    )
    |> Repo.all()
  end

  def view_format_lights(page_id) do
    from(
      l in Light,
      join: c in "page_content_lights",
      on: c.light_id == l.id,
      where: c.page_id == ^page_id,
      preload: [:port, :dimmer],
      select: {c.order, l}
    )
    |> Repo.all()
  end

  def view_format_sunblinds(page_id) do
    from(
      s in Sunblind,
      join: c in "page_content_sunblinds",
      on: c.sunblind_id == s.id,
      where: c.page_id == ^page_id,
      preload: :port,
      select: {c.order, s}
    )
    |> Repo.all()
  end

  def view_format_dimmers(page_id) do
    from(
      d in Dimmer,
      join: c in "page_content_dimmers",
      on: c.dimmer_id == d.id,
      where: c.page_id == ^page_id,
      preload: [:port, lights: [:port]],
      select: {c.order, d}
    )
    |> Repo.all()
  end

  def view_format_actions(page_id) do
    from(
      a in Action,
      join: c in "page_content_actions",
      on: c.action_id == a.id,
      where: c.page_id == ^page_id,
      select: {c.order, a}
    )
    |> Repo.all()
  end

  def view_format_tasks(page_id) do
    from(
      t in Task,
      join: c in "page_content_tasks",
      on: c.task_id == t.id,
      where: c.page_id == ^page_id,
      select: {c.order, t}
    )
    |> Repo.all()
  end
end
