defmodule DB.TaskType do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query


  alias DB.{Repo, TaskType}

  schema "task_types" do
    field(:name, :string)
    field(:module, :string)
    field(:action, :boolean)
    field(:device, :boolean)
  end

  def all() do
    Repo.all(TaskType)
  end

  def all_map() do
    from(t in TaskType,
      select: %{id: t.id, 
        name: t.name, 
        module: t.module}
    )
    |> Repo.all()
  end

end
