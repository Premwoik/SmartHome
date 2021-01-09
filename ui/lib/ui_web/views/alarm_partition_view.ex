defmodule UiWeb.AlarmPartitionView do
  use UiWeb, :view
  alias UiWeb.AlarmPartitionView

  def render("index.json", %{alarm_partitions: alarm_partitions}) do
    %{data: render_many(alarm_partitions, AlarmPartitionView, "alarm_partition.json")}
  end

  def render("show.json", %{alarm_partition: alarm_partition}) do
    %{data: render_one(alarm_partition, AlarmPartitionView, "alarm_partition.json")}
  end

  def render("alarm_partition.json", %{alarm_partition: alarm_partition}) do
    %{
      id: alarm_partition.id,
      name: alarm_partition.name,
      number: alarm_partition.number,
      status: alarm_partition.status,
      device_id: alarm_partition.device_id,
      ref: alarm_partition.ref
    }
  end
end
