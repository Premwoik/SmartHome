defmodule DB.Action do
  alias DB.{CRUD, Action, Port, Device}

  @type args :: [CRUD.foreign(Port | Device)]
  @type t :: %Action{
          id: CRUD.id(),
          name: String.t(),
          function: String.t(),
          active: boolean,
          port_id: CRUD.foreign(Port),
          params: map,
          arguments: args,
          timeout: integer,
          start_time: Time.t() | nil,
          end_time: Time.t() | nil,
          ref: CRUD.ref()
        }

  use Memento.Table,
    attributes: [
      :id,
      :name,
      :function,
      :active,
      :port_id,
      :params,
      :arguments,
      :timeout,
      :start_time,
      :end_time,
      :ref
    ],
    index: [:name],
    type: :ordered_set,
    autoincrement: true

  use CRUD, default: [ref: 1, active: false, name: "", arguments: [], timeout: 0]

  @spec arguments(%Action{}, :up | :down) :: list
  def arguments(action, state \\ :up)
  def arguments(%{arguments: nil}, _), do: nil

  def arguments(%{arguments: a}, state) do
    keys = List.wrap(Keyword.get(a, :up_down, [])) ++ List.wrap(Keyword.get(a, state, []))

    Memento.transaction(fn ->
      Enum.map(keys, fn {:foreign, mod, id} -> mod.get_q(id) end)
    end)
    |> CRUD.unwrap()
  end

  def param(action, name, default \\ nil) do
    Map.get(action, :params, %{}) |> Map.get(name, default)
  end

  def get_by_activator([]), do: []

  def get_by_activator(ports) do
    ids = Enum.map(ports, &{:foreign, DB.Port, &1.id})

    Enum.filter(all_active(), &(&1.port_id in ids))
    |> Enum.map(& &1.id)

    #    find_raw(&(&1), Enum.map(ports, & {:==, :"$5", &1.id}), [:"$1"])
  end

  def all_active() do
    find({:==, :active, true})
  end

  def get_active(ids) when is_list(ids) do
    all_active() |> Enum.filter(&(&1.id in ids))
  end

  def get_active(id), do: get_active([id])
end
