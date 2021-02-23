defmodule Ui.AlarmPartitionAdmin do
  @moduledoc """
  The Admin context.
  """
  #
  #  import Ecto.Query, warn: false
  #  alias DB.Repo
  #
  #  alias DB.AlarmPartition
  #
  #  @doc """
  #  Returns the list of alarm_partitions.
  #
  #  ## Examples
  #
  #      iex> list_alarm_partitions()
  #      [%AlarmPartition{}, ...]
  #
  #  """
  #  def list_alarm_partitions do
  #    Repo.all(AlarmPartition)
  #  end
  #
  #  @doc """
  #  Gets a single alarm_partition.
  #
  #  Raises `Ecto.NoResultsError` if the Alarm partition does not exist.
  #
  #  ## Examples
  #
  #      iex> get_alarm_partition!(123)
  #      %AlarmPartition{}
  #
  #      iex> get_alarm_partition!(456)
  #      ** (Ecto.NoResultsError)
  #
  #  """
  #  def get_alarm_partition!(id), do: Repo.get!(AlarmPartition, id) |> Repo.preload(:device)
  #
  #  @doc """
  #  Creates a alarm_partition.
  #
  #  ## Examples
  #
  #      iex> create_alarm_partition(%{field: value})
  #      {:ok, %AlarmPartition{}}
  #
  #      iex> create_alarm_partition(%{field: bad_value})
  #      {:error, %Ecto.Changeset{}}
  #
  #  """
  #  def create_alarm_partition(attrs \\ %{}) do
  #    %AlarmPartition{}
  #    |> AlarmPartition.changeset(attrs, _all_str = true)
  #    |> Repo.insert()
  #  end
  #
  #  @doc """
  #  Updates a alarm_partition.
  #
  #  ## Examples
  #
  #      iex> update_alarm_partition(alarm_partition, %{field: new_value})
  #      {:ok, %AlarmPartition{}}
  #
  #      iex> update_alarm_partition(alarm_partition, %{field: bad_value})
  #      {:error, %Ecto.Changeset{}}
  #
  #  """
  #  def update_alarm_partition(%AlarmPartition{} = alarm_partition, attrs) do
  #    alarm_partition
  #    |> AlarmPartition.changeset(attrs, _all_str = true)
  #    |> Repo.update()
  #  end
  #
  #  @doc """
  #  Deletes a AlarmPartition.
  #
  #  ## Examples
  #
  #      iex> delete_alarm_partition(alarm_partition)
  #      {:ok, %AlarmPartition{}}
  #
  #      iex> delete_alarm_partition(alarm_partition)
  #      {:error, %Ecto.Changeset{}}
  #
  #  """
  #  def delete_alarm_partition(%AlarmPartition{} = alarm_partition) do
  #    Repo.delete(alarm_partition)
  #  end
  #
  #  @doc """
  #  Returns an `%Ecto.Changeset{}` for tracking alarm_partition changes.
  #
  #  ## Examples
  #
  #      iex> change_alarm_partition(alarm_partition)
  #      %Ecto.Changeset{source: %AlarmPartition{}}
  #
  #  """
  #  def change_alarm_partition(%AlarmPartition{} = alarm_partition) do
  #    AlarmPartition.changeset(alarm_partition, %{})
  #  end
end
