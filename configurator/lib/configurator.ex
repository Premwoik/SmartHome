defmodule Configurator do
  @moduledoc """
  Documentation for Configurator.
  """
  @server Application.get_env(:configurator, :server)

  def send_fn(fun) do
    t = Task.Supervisor.async {Configurator.TaskSupervisor, @server}, fun
    Task.await(t)
  end

  defmacro send(x) do
    quote do
      fn -> unquote(x) |> Core.handle_update() end |> Configurator.send_fn()
    end
  end
end
