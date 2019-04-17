defmodule DB.DeviceType do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  schema "device_types" do
    field(:name, :string)
    field(:module, :string)
  end
end
