defmodule Ui.ThermometerAdmin do
  @moduledoc """
  The Admin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo
  alias DB.Thermometer

  @doc """
  Returns the list of thermometers.

  ## Examples

      iex> list_thermometers()
      [%Thermometer{}, ...]

  """
  def list_thermometers do
    Repo.all(Thermometer)
    |> Enum.map(
         fn therm ->
           %Thermometer{
             readings: from(
                         r in DB.Thermometer.Read,
                         where: r.id == ^therm.id,
                         order_by: [
                           desc: r.id
                         ],
                         limit: 1
                       )
                       |> Repo.one()
                       |> List.wrap()
           }
         end
       )
  end

  #  def list_thermometers do
  #    Repo.all(Thermometer)
  #  end

  @doc """
  Gets a single thermometer.

  Raises `Ecto.NoResultsError` if the Thermometer does not exist.

  ## Examples

      iex> get_thermometer!(123)
      %Thermometer{}

      iex> get_thermometer!(456)
      ** (Ecto.NoResultsError)

  """
  def get_thermometer!(id), do: Repo.get!(Thermometer, id)

  @doc """
  Creates a thermometer.

  ## Examples

      iex> create_thermometer(%{field: value})
      {:ok, %Thermometer{}}

      iex> create_thermometer(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_thermometer(attrs \\ %{}) do
    %Thermometer{}
    |> Thermometer.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a thermometer.

  ## Examples

      iex> update_thermometer(thermometer, %{field: new_value})
      {:ok, %Thermometer{}}

      iex> update_thermometer(thermometer, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_thermometer(%Thermometer{} = thermometer, attrs) do
    thermometer
    |> Thermometer.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Thermometer.

  ## Examples

      iex> delete_thermometer(thermometer)
      {:ok, %Thermometer{}}

      iex> delete_thermometer(thermometer)
      {:error, %Ecto.Changeset{}}

  """
  def delete_thermometer(%Thermometer{} = thermometer) do
    Repo.delete(thermometer)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking thermometer changes.

  ## Examples

      iex> change_thermometer(thermometer)
      %Ecto.Changeset{source: %Thermometer{}}

  """
  def change_thermometer(%Thermometer{} = thermometer) do
    Thermometer.changeset(thermometer, %{})
  end
end
