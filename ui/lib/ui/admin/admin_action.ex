defmodule Ui.ActionAdmin do
  @moduledoc false

  import Ecto.Query, warn: false
  alias DB.Repo
  alias DB.Action

  @doc """
  Returns the list of actions.

  ## Examples

      iex> list_actions()
      [%Action{}, ...]

  """
  def list_actions do
    Repo.all Action
  end

  @doc """
  Gets a single action.

  Raises if the Action does not exist.

  ## Examples

      iex> get_action!(123)
      %Action{}

  """
  def get_action!(id), do: Repo.get(Action, id)

  @doc """
  Creates a action.

  ## Examples

      iex> create_action(%{field: value})
      {:ok, %Action{}}

      iex> create_action(%{field: bad_value})
      {:error, ...}

  """
  def create_action(attrs \\ %{}) do
    %Action{}
    |> Action.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a action.

  ## Examples

      iex> update_action(action, %{field: new_value})
      {:ok, %Action{}}

      iex> update_action(action, %{field: bad_value})
      {:error, ...}

  """
  def update_action(%Action{} = action, attrs) do
    action
    |> Action.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Action.

  ## Examples

      iex> delete_action(action)
      {:ok, %Action{}}

      iex> delete_action(action)
      {:error, ...}

  """
  def delete_action(%Action{} = action) do
    Repo.delete action
  end

  @doc """
  Returns a data structure for tracking action changes.

  ## Examples

      iex> change_action(action)
      %Todo{...}

  """
  def change_action(%Action{} = action) do
    Action.changeset(action, %{})
  end

end
