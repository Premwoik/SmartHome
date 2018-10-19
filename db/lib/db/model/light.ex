defmodule DB.Light do
  @moduledoc false

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  #  @primary_key {:port, :id, []}
#  @derive {Poison.Encoder, only: [:id, :name, :fill, :lights]}
  @primary_key false
  schema "lights" do
    belongs_to :port, DB.Port
    belongs_to :dimmer, DB.Dimmer
  end

  def get(ids) do
    DB.Repo.all from l in DB.Light, where: l.port_id in ^ids, preload: [:port, dimmer: [:port]]
  end

  def group_by_dimmer(lights) do
    lights
    |> Enum.group_by(fn light -> light.dimmer end)
  end

  def dim_light?(light) do
    light.dimmer != nil
  end

end
