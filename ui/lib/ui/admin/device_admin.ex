defmodule Ui.DeviceAdmin do
  @moduledoc """
  The Admin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo
  alias DB.{Device, DeviceType}

  def list_devices do
    Repo.all(Device)
    |> Repo.preload(:type)
  end


  def get_device!(id),
      do: Repo.get!(Device, id)
          |> Repo.preload(:type)


  def create_device(attrs \\ %{}) do
    %Device{}
    |> Device.changeset(attrs, all_str = true)
    |> Repo.insert()
  end

  def update_device(%Device{} = device, attrs) do
    device
    |> Device.changeset(attrs, all_str = true)
    |> Repo.update()
  end

  def delete_device(%Device{} = device) do
    Repo.delete(device)
  end


  def change_device(%Device{} = device) do
    Device.changeset(device, %{})
  end

  def get_types do
    Repo.all(DeviceType)
  end


end
