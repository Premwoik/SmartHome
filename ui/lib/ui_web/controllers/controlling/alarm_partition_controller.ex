defmodule UiWeb.AlarmPartitionController do
  use UiWeb, :controller

  alias Ui.AlarmPartitionAdmin, as: Admin
  alias DB.AlarmPartition

  action_fallback UiWeb.FallbackController

  def prepare_partition(id, password) do
    partition = Admin.get_alarm_partition!(id)
    %{partition | device: %{partition.device | password: to_string(password)}}
  end

  def arm(conn, %{"id" => id, "password" => password, "mode" => mode}) do
    p = prepare_partition(id, password)

    res =
      Core.Device.do_(:arm, [p.device, mode, [p.number]])
      |> IO.inspect()

    send_resp(conn, 200, "#{inspect(res)}")
  end

  def disarm(conn, %{"id" => id, "password" => password}) do
    p = prepare_partition(id, password)

    res =
      Core.Device.do_(:disarm, [p.device, [p.number]])
      |> IO.inspect()

    send_resp(conn, 200, "#{inspect(res)}")
  end

  def clear_alarm(conn, %{"id" => id, "password" => password}) do
    p = prepare_partition(id, password)

    res =
      Core.Device.do_(:clear_alarm, [p.device, [p.number]])
      |> IO.inspect()

    send_resp(conn, 200, "#{inspect(res)}")
  end

  def index(conn, _params) do
    alarm_partitions = Admin.list_alarm_partitions()
    render(conn, "index.json", alarm_partitions: alarm_partitions)
  end

  def create(conn, %{"alarm_partition" => alarm_partition_params}) do
    with {:ok, %AlarmPartition{} = alarm_partition} <-
           Admin.create_alarm_partition(alarm_partition_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.alarm_partition_path(conn, :show, alarm_partition))
      |> render("show.json", alarm_partition: alarm_partition)
    end
  end

  def show(conn, %{"id" => id}) do
    alarm_partition = Admin.get_alarm_partition!(id)
    render(conn, "show.json", alarm_partition: alarm_partition)
  end

  def update(conn, %{"id" => id, "alarm_partition" => alarm_partition_params}) do
    alarm_partition = Admin.get_alarm_partition!(id)

    with {:ok, %AlarmPartition{} = alarm_partition} <-
           Admin.update_alarm_partition(alarm_partition, alarm_partition_params) do
      render(conn, "show.json", alarm_partition: alarm_partition)
    end
  end

  def delete(conn, %{"id" => id}) do
    alarm_partition = Admin.get_alarm_partition!(id)

    with {:ok, %AlarmPartition{}} <- Admin.delete_alarm_partition(alarm_partition) do
      send_resp(conn, :no_content, "")
    end
  end
end
