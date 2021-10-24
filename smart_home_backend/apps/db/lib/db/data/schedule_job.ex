defmodule DB.Data.ScheduleJob do
  @moduledoc """
  The schedule job data.
  """

  use Ecto.Schema
  import Ecto.Changeset

  alias DB.Data.ScheduleJob
  alias DB.MainRepo

  @typedoc """
  FIXME add doc for fields
  """

  @type t :: %ScheduleJob{
          id: integer(),
          name: String.t(),
          expr: String.t(),
          extended: boolean(),
          status: boolean(),
          task: map()
        }

  schema "schedule_jobs" do
    field(:name, :string)
    field(:expr, :string)
    field(:extended, :boolean, default: false)
    field(:status, :boolean, default: true)
    field(:task, :map)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :expr, :task])
  end

  def get!(id) do
    MainRepo.get(ScheduleJob, id)
  end

  def list_all() do
    {:ok, list_all!()}
  end

  def list_all!() do
    MainRepo.all(ScheduleJob)
  end

  def update(id, params) do
    job = get!(id)
    cs = changeset(job, params)

    case cs do
      %{changes: ch} when ch == %{} ->
        :ok

      _ ->
        MainRepo.update(cs)
    end
  end
end
