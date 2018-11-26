defmodule Ui.Admin do
  @moduledoc """
  The Admin context.
  """

  import Ecto.Query, warn: false
  alias DB.Repo
  alias DB.Device

  def list_devices do
    Repo.all(Device)
  end


  def get_device!(id), do: Repo.get!(Device, id)


  def create_device(attrs \\ %{}) do
    %Device{}
    |> Device.changeset(attrs)
    |> Repo.insert()
  end

  def update_device(%Device{} = device, attrs) do
    device
    |> Device.changeset(attrs)
    |> Repo.update()
  end

  def delete_device(%Device{} = device) do
    Repo.delete(device)
  end


  def change_device(%Device{} = device) do
    Device.changeset(device, %{})
  end



end
