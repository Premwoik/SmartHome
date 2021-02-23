defmodule Ui.DeviceAdmin do
  @moduledoc """
  The Admin context.
  """

  alias DB.{Repo, Device}

  def list_devices do
    Repo.all(Device)
  end

  def get_device!(id), do: Repo.get(Device, id)

  def get_device(id), do: Repo.get(Device, id)

  def create_device(attrs \\ %{}) do
    Device.new()
    |> Device.cast(attrs)
    |> Repo.insert()
  end

  def update_device(%Device{} = device, attrs) do
    Device.cast(device, attrs)
    |> Repo.update()
  end

  def delete_device(%Device{} = device) do
    Repo.delete(device)
  end

  def change_device(%Device{} = device) do
    #    TODO
    Device.cast(device, %{})
  end

  #  def get_types do
  #    Repo.all(DeviceType)
  #  end
end
