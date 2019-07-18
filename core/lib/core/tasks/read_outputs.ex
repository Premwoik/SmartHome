defmodule Core.Tasks.ReadOutputs do
  @moduledoc false

  @behaviour Core.Tasks.Task
  alias DB.{Task, Port, Light, Repo}
  require Logger

  alias Core.Controllers.BasicController, as: Basics
  alias UiWeb.DashboardChannel.Helper, as: Channel

  @impl true
  def execute(%Task{device: device} = task, %{last_read: last_read}) do
    case Basics.read_outputs(device) do
      {:ok, data} ->
        if data == last_read do
          Logger.debug(
            "[Task|ReadOutputs] Device #{device.name}, id: #{device.id} outputs don't changed!"
          )

          :ok
        else
          update_out_of_date(device, data -- last_read)
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
        Logger.debug(
          "[Task|ReadOutputs] All outputs are up to date! device: #{device.name}, id: #{device.id}"
        )

      {a, nil} ->
        Logger.info(
          "Updated '#{a}' ports belongs to device with id: '#{device.id}', name: '#{device.name}'"
        )

        update_owning_lights(device)

      _err ->
        :err
    end
  end

  defp update_owning_lights(device) do
    Repo.preload(device, :ports).ports
    |> Enum.filter(fn p -> p.type == "light" end)
    |> Enum.each(fn l ->
      {:ok, %{id: id, ref: ref}} =
        Repo.preload(l, :light).light
        |> DB.Light.update_one()

      Channel.broadcast_change("light", id, ref)
    end)
  end
end
