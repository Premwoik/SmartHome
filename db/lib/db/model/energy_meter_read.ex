defmodule DB.EnergyMeter.Read do
  @moduledoc false
  use Ecto.Schema
  #  import Ecto.Changeset
  import Ecto.Query
  alias DB.EnergyMeter.Read
  alias DB.{Repo}

  schema "wattmeter_reads" do
    belongs_to(:wattmeter, DB.EnergyMeter)
    field(:value, :integer)
    timestamps()
  end

  def find(id, limit \\ 24) do
    from(
      r in Read,
      where: r.wattmeter_id == ^id,
      order_by: [
        desc: r.inserted_at
      ],
      limit: ^limit
    )
    |> Repo.all()
  end
end
