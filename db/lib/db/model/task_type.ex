defmodule DB.TaskType do
  @moduledoc false
  use Ecto.Schema

  schema "task_types" do
    field(:name, :string)
    field(:module, :string)
  end
end
