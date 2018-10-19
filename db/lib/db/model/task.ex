defmodule DB.Task do
  @moduledoc false
  use Ecto.Schema
  use Timex.Ecto.Timestamps
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Repo, Task}

  schema "tasks" do
    belongs_to :type, DB.TaskType
    field :status, :string
    belongs_to :action, DB.Action
    belongs_to :device, DB.Device
    field :frequency, :integer
    field :limit, :integer
    field :start_date, :naive_datetime
    field :end_date, :naive_datetime
  end


  def all() do
    Repo.all Task
  end

  def get_active() do
    from(t in Task, where: t.status != "inactive", preload: [:type, :device, :action])
    |> Repo.all()
  end

  def update(task, changes \\ %{}) do
    Ecto.Changeset.change(task, changes)
    |> Repo.update!
  end

  def update_status(ids, status) do
    from(t in Task, where: t.id in ^ids)
    |> Repo.update_all(set: [status: status])
  end

end
