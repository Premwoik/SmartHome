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

  alias DB.Proc.ActionListProc
  alias DB.Proc.DeviceListProc
  alias DB.Proc.PortListProc

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
    MainRepo.get(Page, page_id)
    |> MainRepo.preload(meters: [])
    |> preload_from_proc()
  end

  def lights(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :light end)
  end

  def dimmers(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :dimmer end)
    |> Enum.map(fn p ->
      case p.state["light_ids"] do
        nil ->
          p

        ids ->
          lights = DB.Proc.PortListProc.get_ids(ids)
          DB.Data.Port.put_state(p, "lights", lights)
      end
    end)
  end

  def sunblinds(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :sunblind end)
  end

  def motion_sensors(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type == :motion_sensor end)
  end

  def others(%{ports: ports}) do
    Enum.filter(ports, fn p -> p.type in [:custom, :circut] end)
  end

  defp get_port_ids(page_id) do
    from("page_ports",
      where: [page_id: ^page_id],
      select: [:port_id]
    )
    |> MainRepo.all()
    |> Enum.map(fn %{port_id: id} -> id end)
  end

  defp get_action_ids(page_id) do
    from("page_actions",
      where: [page_id: ^page_id],
      select: [:action_id]
    )
    |> MainRepo.all()
    |> Enum.map(fn %{action_id: id} -> id end)
  end

  defp get_device_ids(page_id) do
    from("page_devices",
      where: [page_id: ^page_id],
      select: [:device_id]
    )
    |> MainRepo.all()
    |> Enum.map(fn %{device_id: id} -> id end)
  end

  defp preload_from_proc(nil) do
    nil
  end

  defp preload_from_proc(%Page{id: id} = page) do
    ports = PortListProc.get_ids(get_port_ids(id))
    actions = ActionListProc.get_ids(get_action_ids(id))
    devices = DeviceListProc.get_ids(get_device_ids(id))
    %{page | ports: ports, actions: actions, devices: devices}
  end
end
