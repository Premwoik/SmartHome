defmodule DB.Page do
  @moduledoc false

  alias DB.{CRUD, Action, Port, Meter, Page}

  @type t :: %Page{
          id: CRUD.id(),
          name: String.t(),
          description: String.t(),
          order: integer,
          content: [
            CRUD.foreign(Port) | CRUD.foreign(Action) | CRUD.foreign(Device) | CRUD.foreign(Meter)
          ],
          ref: CRUD.ref()
        }

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :description,
      :order,
      :content,
      :ref
    ],
    index: [:name],
    type: :ordered_set,
    autoincrement: true

  use CRUD, default: [ref: 1, mode: :toggle, page_id: 1]


  def lights(page) do
    Enum.filter(page.content, fn
      %Port{type: :light} -> true
      _ -> false
    end)
  end

  def dimmers(page) do
    Enum.filter(page.content, fn
      %Port{type: t} -> t in [:dimmer, :dimmer2, :dimmer_rgb, :dimmer_rgbw] 
      _ -> false
    end)
  end

  def sunblinds(page) do
    Enum.filter(page.content, fn
      %Port{type: :sunblind} -> true
      _ -> false
    end)
  end
  
  def sensors(page) do
    Enum.filter(page.content, fn
      %Port{type: :motion_sensor} -> true
      _ -> false
    end)
  end
end
