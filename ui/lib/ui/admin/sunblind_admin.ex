defmodule Ui.SunblindAdmin do
  @moduledoc """
  The AdminSunblind context.
  """

  alias DB.{Repo, Port}
  import Ui.Admin

  @doc """
  Returns the list of sunblinds.

  ## Examples

      iex> list_sunblinds()
      [%Sunblind{}, ...]

  """
  def list_sunblinds do
    Port.sunblinds()
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
  def get_sunblind!(id), do: Repo.get(Port, id) |> check_type(:sunblind)

  def get_sunblind(id) do
    case get_sunblind!(id) do
      nil -> {:error, :wrong_id}
      r -> {:ok, r}
    end
  end

  @doc """
  Creates a sunblind.

  ## Examples

      iex> create_sunblind(%{field: value})
      {:ok, %Sunblind{}}

      iex> create_sunblind(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_sunblind(attrs \\ %{}) do
    attrs = split_more_fields(attrs, Port, Port.Sunblind)

    Port.new()
    |> Port.cast(attrs)
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
  def update_sunblind(%Port{} = sunblind, attrs) do
    attrs = split_more_fields(attrs, Port, Port.Sunblind)
    sunblind
    |> Port.cast(attrs)
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
  def delete_sunblind(%Port{} = sunblind) do
    Repo.delete(sunblind)
  end

end
