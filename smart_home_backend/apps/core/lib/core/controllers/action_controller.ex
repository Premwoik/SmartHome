defmodule Core.Controllers.ActionController do
  @moduledoc false

  use Core.Controller

  alias Core.Device.Static.Response
  alias DB.Proc.ActionListProc

  @impl true
  def toggle(items, ops) do
    Enum.map(items, fn i ->
      state = not i.state
      set_state([i], state, ops)
    end)
    |> Response.fold()
  end

  @impl true
  def set_state(items, state, _ops) do
    Enum.map(items, fn i ->
      with {:ok, action} <- ActionListProc.update(i.id, Map.put(i, :state, state)) do
        Response.ok(action)
      else
        error -> Response.error(error, i)
      end
    end)
    |> Response.fold()
  end

  # defmodule Activate do
  # use Core.Controller
  # alias Core.Actions

  # @impl true
  # def turn_on(actions, _ops) do
  # actions = Enum.map(actions, fn x -> x.id end)
  # Actions.activate_up(actions)
  # end

  # @impl true
  # def turn_off(actions, _ops) do
  # actions = Enum.map(actions, fn x -> x.id end)
  # Actions.activate_down(actions)
  # end

  # @impl true
  # def toggle(actions, _ops) do
  # actions = Enum.map(actions, fn x -> x.id end)
  # Actions.activate_up(actions)
  # end
  # end
end
