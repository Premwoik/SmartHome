defmodule DB.House do
  @moduledoc false
  #
  #  alias DB.{RfButton, CRUD, Action, Port, Meter, House}
  #
  #  defmodule Floor do
  #    alias DB.Mnesia.House.Floor
  #    #    @type t :: %Floor{name: String.t, rooms: []}
  #  end
  #
  #  defmodule Room do
  #    alias DB.Mnesia.House.Room
  #    #    @type t :: %Room{name: String.t, sensors: [], devices: []}
  #  end
  #
  #  @type t :: %House{
  #          id: CRUD.id(),
  #          name: String.t(),
  #          description: String.t(),
  #          floors: [
  #            CRUD.foreign(Port) | CRUD.foreign(Action) | CRUD.foreign(Device) | CRUD.foreign(Meter)
  #          ],
  #          ref: CRUD.ref()
  #        }
  #
  #  use Memento.Table,
  #    attributes: [
  #      :id,
  #      :name,
  #      :description,
  #      :floors,
  #      :ref
  #    ],
  #    index: [:name],
  #    type: :ordered_set,
  #    autoincrement: true
  #
  #  use CRUD, default: [ref: 1, mode: :toggle, page_id: 1]
end
