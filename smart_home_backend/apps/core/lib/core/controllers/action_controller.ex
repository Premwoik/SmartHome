defmodule Core.ActionController do
  @moduledoc false

  use Core.Controller

  alias Core.Actions
  alias Core.Device.Static.Response

  @impl true
  def toggle(items, ops) do
    Enum.map(items, fn i ->
      state = not i.state
      set_state([i], state, ops)
    end)
    |> Response.fold()
  end

  @impl true
  def set_state(actions, state, _ops) do
    ids = Enum.map(actions, fn x -> x.id end)

    res =
      if state do
        Actions.activate_up(ids)
      else
        Actions.activate_down(ids)
      end

    case res do
      :error ->
        Response.error({:error, :fatal}, actions)

      {:ok, []} ->
        Response.ok(actions)

      {:ok, errors} ->
        errors = Enum.map(errors, fn {:error, pair} -> pair end)
        err_ids = Enum.map(errors, fn {id, _} -> id end)
        oks = Enum.filter(actions, fn a -> a.id not in err_ids end)

        %Response{ok: oks, error: errors, result: [:error]}
    end
  end
end
