defmodule DB.Thermometer.Read do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.{Thermometer, Repo}
  alias DB.Thermometer.Read, as: Read

  schema "therm_temp_reads" do
    belongs_to(:therm, DB.Thermometer)
    field(:value, :float)
    timestamps()
  end

  def find(id, limit \\ 24) do
    from(
      r in Read,
      where: r.therm_id == ^id,
      order_by: [
        desc: r.inserted_at
      ],
      limit: ^limit
    )
    |> Repo.all()
  end

end
