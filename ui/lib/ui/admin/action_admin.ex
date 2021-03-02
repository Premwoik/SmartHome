defmodule Ui.ActionAdmin do
  @moduledoc false

  alias DB.{Repo, Action}

  @doc """
  Returns the list of actions.

  ## Examples

      iex> list_actions()
      [%Action{}, ...]

  """
  def list_actions do
    Repo.all(Action)
  end

  @doc """
  Gets a single action.

  Raises if the Action does not exist.

  ## Examples

      iex> get_action!(123)
      %Action{}

  """
  def get_action!(id), do: Repo.get(Action, id)

  def get_action(id) do
    case Repo.get(Action, id) do
      nil -> {:error, :wrong_id}
      res -> {:ok, res}
    end
  end

  @doc """
  Creates a action.

  ## Examples

      iex> create_action(%{field: value})
      {:ok, %Action{}}

      iex> create_action(%{field: bad_value})
      {:error, ...}

  """
  def create_action(attrs \\ %{}) do
    action =
      Action.new()
      |> Action.cast(attrs)
      |> Repo.insert()

    {:ok, action}
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
    |> Action.cast(attrs)
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
    Repo.delete(action)
  end


  def update_action_args(id, port_ids) do
    Action.get(id) |> Action.cast(arguments: [up_down: DB.Init.foreign(DB.Port, port_ids)])
    :ok
  end

  def get_action_items(id) do
    Action.arguments(Action.get(id), :up_down)
  end
end
