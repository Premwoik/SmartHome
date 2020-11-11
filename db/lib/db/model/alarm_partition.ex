defmodule DB.AlarmPartition do
  use Ecto.Schema
  import Ecto.Changeset
  import DB


  schema "alarm_partitions" do
    field :name, :string
    field :number, :integer
    field :status, :string
    field :ref, :integer
    belongs_to(:device, DB.Device)

  end

  @doc false
  def changeset(alarm_partition, attrs) do
    alarm_partition
    |> cast(attrs, [:name, :number, :status, :device_id])
  end

  def changeset(port, params \\ %{}, all_str \\ false) do
    params_ = inc_ref(port, Enum.into(params, %{}), all_str)
    port
    |> cast(params_, [:state, :number, :device_id, :status, :ref])
    |> validate_required([:name, :number, :status, :device_id])
  end
end
