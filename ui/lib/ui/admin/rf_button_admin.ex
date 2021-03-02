defmodule Ui.RfButtonAdmin do
  @moduledoc """
  The Admin context.
  """

  alias DB.{Repo, RfButton}

  @doc """
  Returns the list of rf_buttons.

  ## Examples

      iex> list_rf_buttons()
      [%RfButton{}, ...]

  """
  def list_rf_buttons do
    Repo.all(RfButton)
  end

  @doc """
  Gets a single rf_button.

  Raises `Ecto.NoResultsError` if the Rf button does not exist.

  ## Examples

      iex> get_rf_button!(123)
      %RfButton{}

      iex> get_rf_button!(456)
      ** (Ecto.NoResultsError)

  """
  def get_rf_button!(id), do: Repo.get(RfButton, id)

  @doc """
  Creates a rf_button.

  ## Examples

      iex> create_rf_button(%{field: value})
      {:ok, %RfButton{}}

      iex> create_rf_button(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_rf_button(attrs \\ %{}) do
    RfButton.new()
    |> RfButton.cast(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a rf_button.

  ## Examples

      iex> update_rf_button(rf_button, %{field: new_value})
      {:ok, %RfButton{}}

      iex> update_rf_button(rf_button, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_rf_button(%RfButton{} = rf_button, attrs) do
    rf_button
    |> RfButton.cast(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a RfButton.

  ## Examples

      iex> delete_rf_button(rf_button)
      {:ok, %RfButton{}}

      iex> delete_rf_button(rf_button)
      {:error, %Ecto.Changeset{}}

  """
  def delete_rf_button(%RfButton{} = rf_button) do
    Repo.delete(rf_button)
  end

end
