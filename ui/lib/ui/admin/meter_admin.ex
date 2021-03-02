defmodule Ui.MeterAdmin do
  @moduledoc """
  The Admin context.
  """

  alias DB.{Repo, Meter}

  @doc """
  Returns the list of meter.

  ## Examples

      iex> list_meter()
      [%Meter{}, ...]

  """
  def list_meters do
    Meter.all()
    |> Enum.map(fn meter ->
      case meter.type do
        :temperature ->
          Map.put(meter, :readings, DB.Stats.Temperature.get(meter.id, :hourly, limit: 1))
        :energy ->
          Map.put(meter, :readings, DB.Stats.Energy.get(meter.id, :hourly, limit: 1))
      end
    end)
  end

  def list_thermometers do
    Meter.thermometers()
    |> Enum.map(fn meter ->
      Map.put(meter, :readings, DB.Stats.Temperature.get(meter.id, :hourly, limit: 1))
    end)
  end

  def list_energy_meters do
    Meter.energy_meters()
    |> Enum.map(fn meter ->
      Map.put(meter, :readings, DB.Stats.Energy.get(meter.id, :hourly, limit: 1))
    end)
  end

  @doc """
  Gets a single meter.

  Raises `Ecto.NoResultsError` if the Energy meter does not exist.

  ## Examples

      iex> get_meter!(123)
      %Meter{}

      iex> get_meter!(456)
      ** (Ecto.NoResultsError)

  """
  def get_meter!(id), do: Repo.get(Meter, id)

  @doc """
  Creates a meter.

  ## Examples

      iex> create_meter(%{field: value})
      {:ok, %Meter{}}

      iex> create_meter(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_meter(attrs \\ %{}) do
    Meter.new()
    |> Meter.cast(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a meter.

  ## Examples

      iex> update_meter(meter, %{field: new_value})
      {:ok, %Meter{}}

      iex> update_meter(meter, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_meter(%Meter{} = meter, attrs) do
    meter
    |> Meter.cast(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Meter.

  ## Examples

      iex> delete_meter(meter)
      {:ok, %Meter{}}

      iex> delete_meter(meter)
      {:error, %Ecto.Changeset{}}

  """
  def delete_meter(%Meter{} = meter) do
    Repo.delete(meter)
  end
end
