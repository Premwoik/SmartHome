defmodule DB.DimmersLights do
  @moduledoc false
  use Ecto.Schema

  @primary_key {:dimmer_id, :id, []}
  schema "dimmers_lights" do
    field :port_id, :id
  end
end
