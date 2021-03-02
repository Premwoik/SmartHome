defmodule DB.ScheduleJob do
  @moduledoc false

  alias DB.{ScheduleJob, CRUD, Action}

  @type t :: %ScheduleJob{
          id: CRUD.id(),
          expr: String.t(),
          active: boolean,
          extended: boolean,
          state: boolean,
          action_id: {:foreign, Action, CRUD.id()},
          ref: CRUD.id()
        }

  use Memento.Table,
    attributes: [
      :id,
      :expr,
      :active,
      :extended,
      :state,
      :action_id,
      :ref
    ],
    #      index: [:name],
    type: :ordered_set,
    autoincrement: true

  use CRUD, default: [ref: 1, active: true, extended: false, state: :down]
end
