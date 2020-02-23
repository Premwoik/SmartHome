defmodule Ui.DimmerAdmin do
  @moduledoc """
  The DimmerAdmin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo
  alias Ui.PortAdmin
  alias DB.{Port,Dimmer}

  @doc """
  Returns the list of dimmers.

  ## Examples

      iex> list_dimmers()
      [%Dimmer{}, ...]

  """
  def list_dimmers do
    Repo.all(Dimmer) |> preload()
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
  def get_dimmer!(id), do: Repo.get!(Dimmer, id) |> preload() 

  def get_dimmer(id) do
    res = Repo.get(Dimmer, id) |> preload() 
    case res do
      nil -> {:error, :wrong_id}
      r -> {:ok, r}
    end
  end


  def preload(dimmer), do: Repo.preload(dimmer, [port: [:device], lights: [:port]])


  @doc """
  Creates a dimmer.

  ## Examples

      iex> create_dimmer(%{field: value})
      {:ok, %Dimmer{}}

      iex> create_dimmer(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_dimmer(attrs \\ %{}) do
    IO.inspect(attrs)
    %Dimmer{}
    |> Dimmer.changeset(attrs, all_str = true)
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
  def update_dimmer(%Dimmer{} = dimmer, attrs) do
    dimmer
    |> Dimmer.changeset(attrs, all_str = true)
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
  def delete_dimmer(%Dimmer{} = dimmer) do
    Repo.delete(dimmer)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking dimmer changes.

  ## Examples

      iex> change_dimmer(dimmer)
      %Ecto.Changeset{source: %Dimmer{}}

  """
  def change_dimmer(%Dimmer{} = dimmer) do
    Dimmer.changeset(dimmer, %{})
  end
end
