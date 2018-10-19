defmodule DB.Device do
  use Ecto.Schema
  @moduledoc false

  schema "devices" do
  field :name, :string
  field :ip, :string
  field :port, :integer
  field :type, :string
  field :process, :boolean, default: true
  has_many :ports, DB.Port
  has_one :watcher, DB.Watcher
  end

  def get(id) do
    DB.Repo.get DB.Device, id
  end

  def all() do
    DB.Repo.all DB.Device
  end


end
