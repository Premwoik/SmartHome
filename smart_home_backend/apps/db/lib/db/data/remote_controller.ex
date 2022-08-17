defmodule DB.Data.RemoteController do
  use Ecto.Schema
  import Ecto.Changeset

  alias DB.Data.RemoteController
  alias DB.Data.RfButton
  alias DB.MainRepo

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %RemoteController{
          id: integer(),
          name: String.t(),
          cols: integer()
        }

  schema "remote_controllers" do
    field(:name, :string)
    field(:cols, :integer)
    has_many(:buttons, RfButton)
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :cols])
  end

  def list_all! do
    MainRepo.all(RemoteController)
  end

  def layout(%RemoteController{cols: cols, buttons: buttons}) do
    Enum.chunk_every(buttons, cols)
  end
end
