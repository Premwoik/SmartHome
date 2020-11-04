defmodule DB.Watcher do
  @moduledoc false
  use Ecto.Schema

  schema "watchers" do
    belongs_to(:device, DB.Device)
    field(:status, :boolean)
    field(:freq, :integer)
  end
end
