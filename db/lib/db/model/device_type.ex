defmodule DB.DeviceType do
  @moduledoc false
  use Ecto.Schema

  schema "device_types" do
    field(:name, :string)
    field(:module, :string)
    field(:process, :boolean, default: true)
  end
end
