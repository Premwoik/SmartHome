defmodule Ui.LightAdmin do
  @moduledoc """
  The LightAdmin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo

  alias DB.Light
  alias DB.Port
  alias DB.Dimmer

  @doc """
  Returns the list of lights.

  ## Examples

      iex> list_lights()
      [%Light{}, ...]

  """
  def list_lights do
    Repo.all(Light)
    |> preload()
  end

  @doc """
  Gets a single light.

  Raises `Ecto.NoResultsError` if the Light does not exist.

  ## Examples

      iex> get_light!(123)
      %Light{}

      iex> get_light!(456)
      ** (Ecto.NoResultsError)

  """
  def get_light!(id),
      do: Repo.get!(Light, id)
          |> preload()

  def get_light(id) do
    res = Repo.get(Light, id)
          |> preload()
    case res do
      nil -> {:error, :wrong_id}
      r -> {:ok, r}
    end
  end


  def preload(light) do
    Repo.preload(light, [:port, dimmer: [:port]])
  end

  @doc """
  Creates a light.

  ## Examples

      iex> create_light(%{field: value})
      {:ok, %Light{}}

      iex> create_light(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_light(attrs \\ %{}) do
    %Light{}
    |> Light.changeset(attrs, all_str = true)
    |> Repo.insert()
  end

  @doc """
  Updates a light.

  ## Examples

      iex> update_light(light, %{field: new_value})
      {:ok, %Light{}}

      iex> update_light(light, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_light(%Light{} = light, attrs) do
    light
    |> Light.changeset(attrs, all_str = true)
    |> Repo.update()
  end

  @doc """
  Deletes a Light.

  ## Examples

      iex> delete_light(light)
      {:ok, %Light{}}

      iex> delete_light(light)
      {:error, %Ecto.Changeset{}}

  """
  def delete_light(%Light{} = light) do
    Repo.delete(light)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking light changes.

  ## Examples

      iex> change_light(light)
      %Ecto.Changeset{source: %Light{}}

  """
  def change_light(%Light{} = light) do
    Light.changeset(light, %{})
  end
end
