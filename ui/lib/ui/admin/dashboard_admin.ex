defmodule Ui.DashboardAdmin do
  @moduledoc """
  The DashboardAdmin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo

  alias DB.{Page, PageContent, Repo, Port, Light, Sunblind, Dimmer, Action, Task, TaskType, Device}
  alias DB.Page, as: Dashboard

  @doc """
  Returns the list of dashboards.

  ## Examples

      iex> list_dashboards()
      [%Dashboard{}, ...]

  """
  def list_dashboards do
    Repo.all(Dashboard)
    |> Enum.map(
         fn d -> %{
                   d |
                   ports: content_ports(d.id),
                   actions: content_actions(d.id),
                   dimmers: content_dimmers(d.id),
                   lights: content_lights(d.id),
                   sunblinds: content_sunblinds(d.id),
                   tasks: content_tasks(d.id),
                   devices: content_devices(d.id)
                 }
         end
       )
  end

  def list_dashboards_short do
    Repo.all(Dashboard)
    |> Enum.sort_by(fn x -> x.order end)
  end

  @doc """
  Gets a single dashboard.

  Raises `Ecto.NoResultsError` if the Dashboard does not exist.

  ## Examples

      iex> get_dashboard!(123)
      %Dashboard{}

      iex> get_dashboard!(456)
      ** (Ecto.NoResultsError)

  """
  def get_dashboard!(id) do
    d = Repo.get!(Dashboard, id)
    %{
      d |
      ports: view_format_ports(id),
      actions: view_format_actions(id),
      dimmers: view_format_dimmers(id),
      lights: view_format_lights(id),
      sunblinds: view_format_sunblinds(id),
      tasks: view_format_tasks(id),
      devices: view_format_devices(id)
    }
  end

  def get_dashboard_short!(id) do
    Repo.get!(Dashboard, id)
  end

  def get_dashboard_admin!(id) do
    d = Repo.get!(Dashboard, id)
    %{
      d |
      ports: content_ports(id),
      actions: content_actions(id),
      dimmers: content_dimmers(id),
      lights: content_lights(id),
      sunblinds: content_sunblinds(id),
      tasks: content_tasks(id),
      devices: content_devices(id)
    }
  end

  @doc """
  Creates a dashboard.

  ## Examples

      iex> create_dashboard(%{field: value})
      {:ok, %Dashboard{}}

      iex> create_dashboard(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_dashboard(attrs \\ %{}) do

    update_content 0, attrs

    %Dashboard{}
    |> Dashboard.changeset(attrs)
    |> Repo.insert()
    |> load_content()
  end

  @doc """
  Updates a dashboard.

  ## Examples

      iex> update_dashboard(dashboard, %{field: new_value})
      {:ok, %Dashboard{}}

      iex> update_dashboard(dashboard, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_dashboard(dashboard, attrs) do
    id = dashboard.id
    from(p in DB.PageContentPort, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentLight, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentDimmer, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentSunblind, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentAction, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentTask, where: p.page_id == ^id)
    |> Repo.delete_all
    from(p in DB.PageContentDevice, where: p.page_id == ^id)
    |> Repo.delete_all

    update_content dashboard.id, attrs

    dashboard
    |> Dashboard.changeset(attrs)
    |> Repo.update()
    |> load_content()
  end

  def load_content({:ok, %{id: id} = dash}) do
    {
      :ok,
      %{
        dash |
        ports: content_ports(id),
        actions: content_actions(id),
        dimmers: content_dimmers(id),
        lights: content_lights(id),
        sunblinds: content_sunblinds(id),
        tasks: content_tasks(id)
      }
    }
  end


  def update_content(
        id,
        %{
          "ports" => ports,
          "lights" => lights,
          "dimmers" => dimmers,
          "sunblinds" => sunblinds,
          "actions" => actions,
          "tasks" => tasks,
          "devices" => devices
        }
      ) do
    Enum.each(Poison.decode!(ports), &(DB.PageContentPort.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(lights), &(DB.PageContentLight.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(dimmers), &(DB.PageContentDimmer.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(sunblinds), &(DB.PageContentSunblind.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(actions), &(DB.PageContentAction.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(tasks), &(DB.PageContentTask.insert_or_update(id, &1)))
    Enum.each(Poison.decode!(devices), &(DB.PageContentDevice.insert_or_update(id, &1)))
  end

  @doc """
  Deletes a Dashboard.

  ## Examples

      iex> delete_dashboard(dashboard)
      {:ok, %Dashboard{}}

      iex> delete_dashboard(dashboard)
      {:error, %Ecto.Changeset{}}

  """
  def delete_dashboard(dashboard) do
    Repo.delete(dashboard)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking dashboard changes.

  ## Examples

      iex> change_dashboard(dashboard)
      %Ecto.Changeset{source: %Dashboard{}}

  """
  def change_dashboard(%Dashboard{} = dashboard) do
    Dashboard.changeset(dashboard, %{})
  end


  def view_format_ports(page_id) do
    from(
      p in Port,
      join: c in "page_content_ports",
      on: c.port_id == p.id,
      where: c.page_id == ^page_id,
      select: {c.order, p}
    )
    |> Repo.all
  end

  def view_format_lights(page_id) do
    from(
      l in Light,
      join: c in "page_content_lights",
      on: c.light_id == l.id,
      where: c.page_id == ^page_id,
      preload: [
        :port,
        dimmer: [:port]
      ],
      select: {c.order, l}
    )
    |> Repo.all
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
    |> Repo.all
  end

  def view_format_dimmers(page_id) do


    from(
      d in Dimmer,
      join: c in "page_content_dimmers",
      on: c.dimmer_id == d.id,
      where: c.page_id == ^page_id,
      preload: [
        :port,
        lights: [:port]
      ],
      select: {c.order, d}
    )
    |> Repo.all
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
      preload: [:type, :action, :device],
      select: {c.order, t}
    )
    |> Repo.all()
  end

  def view_format_devices(page_id) do
    from(
      d in Device,
      join: c in "page_content_devices",
      on: c.device_id == d.id,
      where: c.page_id == ^page_id,
      select: {c.order, d}
    )
    |> Repo.all()
  end


  def content_actions(id) do
    from(c in "page_content_actions", where: c.page_id == ^id, select: [c.action_id, c.order])
    |> Repo.all()
  end
  def content_ports(id) do
    from(c in "page_content_ports", where: c.page_id == ^id, select: [c.port_id, c.order])
    |> Repo.all()
  end
  def content_dimmers(id) do
    from(c in "page_content_dimmers", where: c.page_id == ^id, select: [c.dimmer_id, c.order])
    |> Repo.all()
  end
  def content_devices(id) do
    from(c in "page_content_devices", where: c.page_id == ^id, select: [c.device_id, c.order])
    |> Repo.all()
  end
  def content_lights(id) do
    from(c in "page_content_lights", where: c.page_id == ^id, select: [c.light_id, c.order])
    |> Repo.all()
  end
  def content_sunblinds(id) do
    from(c in "page_content_sunblinds", where: c.page_id == ^id, select: [c.sunblind_id, c.order])
    |> Repo.all()
  end
  def content_tasks(id) do
    from(c in "page_content_tasks", where: c.page_id == ^id, select: [c.task_id, c.order])
    |> Repo.all()
  end

end
