defmodule DB.Device do
  use Ecto.Schema
  @moduledoc false

  schema "devices" do
  field :name, :string
  field :ip, :string
  field :port, :integer
  field :type, :string
  has_many :ports, DB.Port, on_delete: :delete_all
  has_one :watcher, DB.Watcher, on_delete: :delete_all
  end
end
