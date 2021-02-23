defmodule DB.RfButton do
  @moduledoc false

  alias DB.{RfButton, CRUD, Action, Port}

  @type mode :: :on | :off | :toggle | :page
  @type page_id :: integer
  @type t :: %RfButton{
          id: CRUD.id(),
          name: String.t(),
          mode: mode,
          key_value: String.t(),
          page_id: integer,
          on_click_action: [{page_id, CRUD.foreign(Action) | Crud.foreign(Port)}],
          ref: CRUD.ref()
        }

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :mode,
      :key_value,
      :page_id,
      :on_click_action,
      :ref
    ],
    index: [:name],
    type: :ordered_set,
    autoincrement: true

  use CRUD, default: [ref: 1, mode: :toggle, page_id: 1]

  def click_action(button, page) do
    case Map.get(button, :on_click_action) do
      nil -> nil
      actions -> Enum.find(actions, fn {id, _} -> id == page end)
    end
  end

  def identify(key_value) do
    guard = {:==, :key_value, key_value}

    find(guard)
    |> List.first()
  end
end
