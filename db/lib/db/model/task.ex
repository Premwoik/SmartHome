defmodule DB.Task do
  @moduledoc false
  use Ecto.Schema
  use Timex.Ecto.Timestamps
  import Ecto.Changeset
  import Ecto.Query
  import DB

  alias DB.{Repo, Task}

  schema "tasks" do
    belongs_to(:type, DB.TaskType)
    field(:name, :string)
    field(:status, :string)
    belongs_to(:action, DB.Action)
    belongs_to(:device, DB.Device)
    field(:frequency, :integer)
    field(:execution_time, :time)
    field(:limit, :integer)
    field(:start_date, :naive_datetime)
    field(:end_date, :naive_datetime)
    field(:ref, :integer)
  end

  def changeset(task, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(task, Enum.into(params, %{}), all_str)
    task 
    |> cast(params_, [:name, :status, :type_id, :action_id, :device_id, :frequency, :execution_time, :limit, :ref])

    # TODO add others parameters to the cast, only when android app will be seting it correctly
  end

  def all() do
    Repo.all(Task)
  end

  def get_active() do
    from(t in Task, where: t.status != "inactive", preload: [:type, [device: [:type]], :action])
    |> Repo.all()
  end

  def update(task, changes \\ %{}) do
    Ecto.Changeset.change(task, changes)
    |> Repo.update!()
  end

  def update_status(ids, status) do
    from(t in Task, where: t.id in ^ids)
    |> Repo.update_all(set: [status: status])
  end
end
