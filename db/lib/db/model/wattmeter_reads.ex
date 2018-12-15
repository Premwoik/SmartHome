defmodule DB.Wattmeter.Read do
  @moduledoc false
  use Ecto.Schema

  schema "wattmeter_reads" do
    belongs_to :wattmeter, DB.Wattmeter
    field :value, :integer
    timestamps()
  end
end
