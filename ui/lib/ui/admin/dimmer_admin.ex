defmodule Ui.DimmerAdmin do
  @moduledoc """
  The DimmerAdmin context.
  """

  alias DB.{Repo, Port}
  import Ui.Admin

  @doc """
  Returns the list of dimmers.

  ## Examples

      iex> list_dimmers()
      [%Dimmer{}, ...]

  """
  def list_dimmers do
    Port.dimmers()
  end

  @doc """
  Gets a single dimmer.

  Raises `Ecto.NoResultsError` if the Dimmer does not exist.

  ## Examples

      iex> get_dimmer!(123)
      %Dimmer{}

      iex> get_dimmer!(456)
      ** (Ecto.NoResultsError)

  """
  def get_dimmer!(id),
    do: Repo.get(Port, id)

  def get_dimmer(id) do
    case Repo.get(Port, id) do
      nil -> {:error, :wrong_id}
      r -> {:ok, r}
    end
  end

  @doc """
  Creates a dimmer.

  ## Examples

      iex> create_dimmer(%{field: value})
      {:ok, %Dimmer{}}

      iex> create_dimmer(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_dimmer(attrs \\ %{}) do
    attrs = split_more_fields(attrs, Port, Port.Dimmer)
    Port.new()
    |> Port.cast(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a dimmer.

  ## Examples

      iex> update_dimmer(dimmer, %{field: new_value})
      {:ok, %Dimmer{}}

      iex> update_dimmer(dimmer, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_dimmer(%Port{} = dimmer, attrs) do
    attrs = split_more_fields(attrs, Port, Port.Dimmer)
    dimmer
    |> Port.cast(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Dimmer.

  ## Examples

      iex> delete_dimmer(dimmer)
      {:ok, %Dimmer{}}

      iex> delete_dimmer(dimmer)
      {:error, %Ecto.Changeset{}}

  """
  def delete_dimmer(%Port{} = dimmer) do
    Repo.delete(dimmer)
  end
end
