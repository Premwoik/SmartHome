defmodule Ui.LightAdmin do
  @moduledoc """
  The LightAdmin context.
  """

  alias DB.{Repo, Port}
  import Ui.Admin

  @doc """
  Returns the list of lights.

  ## Examples

      iex> list_lights()
      [%Light{}, ...]

  """
  def list_lights do
    Port.lights()
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
  def get_light!(id), do: Repo.get(Port, id)

  def get_light(id) do
    case Repo.get(Port, id) do
      nil -> {:error, :wrong_id}
      r -> {:ok, r}
    end
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
    attrs = split_more_fields(attrs, Port, Port.Light)
    Port.new()
    |> Port.cast(attrs)
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
  def update_light(%Port{} = light, attrs) do
    attrs = split_more_fields(attrs, Port, Port.Light)
    light
    |> Port.cast(attrs)
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
  def delete_light(%Port{} = light) do
    Repo.delete(light)
  end

end
