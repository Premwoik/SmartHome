defmodule DB.Data.Page do
  @moduledoc """
  The page data.
  """

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias DB.MainRepo
  alias DB.Data.Action
  alias DB.Data.Device
  alias DB.Data.Meter
  alias DB.Data.Page
  alias DB.Data.Port

  @typedoc """
  FIXME add doc for fields
  """
  @type t :: %Page{
          id: integer(),
          name: String.t(),
          description: String.t(),
          order: integer(),
          content: nil | [map()],
          actions: [Action],
          devices: [Device],
          meters: [Meter],
          ports: [Port]
        }

  schema "pages" do
    field(:name, :string)
    field(:description, :string)
    field(:order, :integer)
    field(:content, {:array, :map}, virtual: true)

    many_to_many(:ports, Port, join_through: "page_ports")
    many_to_many(:actions, Action, join_through: "page_actions")
    many_to_many(:devices, Device, join_through: "page_devices")
    many_to_many(:meters, Meter, join_through: "page_meters")
  end

  def changeset(schema, params) do
    schema
    |> cast(params, __schema__(:fields))
    |> validate_required([:name, :address, :type, :device_id])
  end

  def list_all() do
    {:ok, list_all!()}
  end

  def list_all!() do
    MainRepo.all(Page)
  end

  def short_info() do
    from(p in Page, select: %{id: p.id, name: p.name})
    |> MainRepo.all()
  end

  def get(page_id) do
    MainRepo.get(Page, page_id) |> MainRepo.preload([ports: [:device], actions: [], devices: [], meters: []])
  end

  def lights(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :light end)
  end

  def dimmers(%{ports: ports}) do
    # FIXME preload lights
    Enum.filter(ports, fn p -> p.type == :dimmer end)
  end

  def sunblinds(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :sunblind end)
  end
end
