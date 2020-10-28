defmodule Ui.Admin do
  @moduledoc """
  The Admin context.
  """

  import Ecto.Query, warn: false
  alias Ui.Repo

  alias Ui.Admin.EnergyMeter

  @doc """
  Returns the list of wattmeters.

  ## Examples

      iex> list_wattmeters()
      [%EnergyMeter{}, ...]

  """
  def list_wattmeters do
    Repo.all(EnergyMeter)
  end

  @doc """
  Gets a single energy_meter.

  Raises `Ecto.NoResultsError` if the Energy meter does not exist.

  ## Examples

      iex> get_energy_meter!(123)
      %EnergyMeter{}

      iex> get_energy_meter!(456)
      ** (Ecto.NoResultsError)

  """
  def get_energy_meter!(id), do: Repo.get!(EnergyMeter, id)

  @doc """
  Creates a energy_meter.

  ## Examples

      iex> create_energy_meter(%{field: value})
      {:ok, %EnergyMeter{}}

      iex> create_energy_meter(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_energy_meter(attrs \\ %{}) do
    %EnergyMeter{}
    |> EnergyMeter.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a energy_meter.

  ## Examples

      iex> update_energy_meter(energy_meter, %{field: new_value})
      {:ok, %EnergyMeter{}}

      iex> update_energy_meter(energy_meter, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_energy_meter(%EnergyMeter{} = energy_meter, attrs) do
    energy_meter
    |> EnergyMeter.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a EnergyMeter.

  ## Examples

      iex> delete_energy_meter(energy_meter)
      {:ok, %EnergyMeter{}}

      iex> delete_energy_meter(energy_meter)
      {:error, %Ecto.Changeset{}}

  """
  def delete_energy_meter(%EnergyMeter{} = energy_meter) do
    Repo.delete(energy_meter)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking energy_meter changes.

  ## Examples

      iex> change_energy_meter(energy_meter)
      %Ecto.Changeset{source: %EnergyMeter{}}

  """
  def change_energy_meter(%EnergyMeter{} = energy_meter) do
    EnergyMeter.changeset(energy_meter, %{})
  end
end
