defmodule DB.Meter do
  alias DB.{CRUD, Meter, Device}

  @type meter_t :: :energy | :temperature
  @type t :: %Meter{
          id: CRUD.id(),
          name: String.t(),
          address: String.t(),
          device_id: CRUD.foreign(Device),
          type: meter_t,
          ref: CRUD.ref()
        }

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :address,
      :device_id,
      :type,
      :ref
    ],
    index: [:name, :address],
    type: :ordered_set,
    autoincrement: true

  use CRUD, default: [ref: 1]

  def thermometers() do
    find({:==, :type, :temperature})
  end

  def energy_meters() do
    find({:==, :type, :energy})
  end

  def new_thermometer() do
    new(type: :temperature)
  end

  def new_energy_meter() do
    new(type: :energy)
  end

  def identify(device_id, addr) do
    device_id = {:foreign, Device, device_id}
    guards = {:and, {:==, :device_id, device_id}, {:==, :addr, addr}}
    find(guards) |> List.first()
  end
end
