defmodule Core.Tasks.ReadOutputs do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias DB.{Task, Port}
  require Logger

  alias Core.Controllers.BasicController, as: Basics

  @impl true
  def execute(%Task{device: device} = task, %{last_read: last_read}) do
    case Basics.read_outputs(device) do
      {:ok, data} ->
        if data == last_read do
          Logger.debug("[Task|ReadOutputs] Device #{device.name}, id: #{device.id} outputs don't changed!")
          :ok
        else
          update_out_of_date(device, data)
          {:ok, %{last_read: data}}
        end
      {:error, error} ->
        :error
    end
  end

  @impl true
  def init_state() do
    %{last_read: []}
  end

  defp update_out_of_date(device, data) do
    case Port.update_out_of_date(device.id, data) do
      {0, nil} ->
        Logger.debug("[Task|ReadOutputs] All outputs are up to date! device: #{device.name}, id: #{device.id}")
      {a, nil} ->
        Logger.info("Updated '#{a}' ports belongs to device with id: '#{device.id}', name: '#{device.name}'")
      _err ->
        :err
    end
  end

end
