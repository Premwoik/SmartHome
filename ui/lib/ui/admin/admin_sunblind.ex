defmodule Ui.AdminSunblind do
  @moduledoc """
  The AdminSunblind context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo

  alias DB.Sunblind
  alias Core.Controller.SunblindController

  @doc """
  Returns the list of sunblinds.

  ## Examples

      iex> list_sunblinds()
      [%Sunblind{}, ...]

  """
  def list_sunblinds do
    Repo.all(Sunblind) |> Repo.preload(:port)
  end

  @doc """
  Gets a single sunblind.

  Raises `Ecto.NoResultsError` if the Sunblind does not exist.

  ## Examples

      iex> get_sunblind!(123)
      %Sunblind{}

      iex> get_sunblind!(456)
      ** (Ecto.NoResultsError)

  """
  def get_sunblind!(id), do: Repo.get!(Sunblind, id) |> Repo.preload(:port)


  @doc """
  Creates a sunblind.

  ## Examples

      iex> create_sunblind(%{field: value})
      {:ok, %Sunblind{}}

      iex> create_sunblind(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_sunblind(attrs \\ %{}) do
    %Sunblind{}
    |> Sunblind.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a sunblind.

  ## Examples

      iex> update_sunblind(sunblind, %{field: new_value})
      {:ok, %Sunblind{}}

      iex> update_sunblind(sunblind, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_sunblind(%Sunblind{} = sunblind, attrs) do
    sunblind
    |> Sunblind.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Sunblind.

  ## Examples

      iex> delete_sunblind(sunblind)
      {:ok, %Sunblind{}}

      iex> delete_sunblind(sunblind)
      {:error, %Ecto.Changeset{}}

  """
  def delete_sunblind(%Sunblind{} = sunblind) do
    Repo.delete(sunblind)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking sunblind changes.

  ## Examples

      iex> change_sunblind(sunblind)
      %Ecto.Changeset{source: %Sunblind{}}

  """
  def change_sunblind(%Sunblind{} = sunblind) do
    Sunblind.changeset(sunblind, %{})
  end
end
