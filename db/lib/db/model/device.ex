defmodule DB.Device do
  alias DB.{CRUD, Device}

  @type t :: %Device{
          name: String.t(),
          ip: String.t(),
          port: integer,
          type: String.t(),
          alive: boolean,
          ref: CRUD.ref()
        }

  use CRUD, default: [alive: false, type: "Default", port: 80, ref: 1]

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :ip,
      :port,
      :type,
      :alive,
      :ref
    ],
    index: [:name],
    type: :ordered_set,
    autoincrement: true

  def get_by_type(type) do
    find({:==, :type, type})
  end

  def get_by_name(name) do
    find({:==, :name, name}) |> List.first()
  end

  def get_from_foreign(%{device_id: {:foreign, DB.Device, id}}) do
    get(id)
  end

  def get_from_foreign(_), do: nil
end
