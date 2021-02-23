defmodule Ui.DashboardAdmin do
  @moduledoc """
  The DashboardAdmin context.
  """

  alias DB.{
    Repo,
    Page,
#    Action,
#    Device
  }

  def list_dashboards do
    Repo.all(Page)
  end

  def list_dashboards_short do
    Repo.all(Page)
    |> Enum.sort_by(fn x -> x.order end)
  end

  def get_dashboard!(id, preload \\ false) do
    page = Repo.get(Page, id)
    if(preload) do
      DB.Repo.preload(page)
    else
      page
    end
  end

  def get_dashboard_short!(id) do
    Repo.get(Page, id)
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
    Page.new()
    |> Page.cast(attrs)
    |> Repo.insert()
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
    dashboard
    |> Page.cast(attrs)
    |> Repo.update()
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
end
